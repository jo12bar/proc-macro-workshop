use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Fields, Ident, Index,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let vis = input.vis;
    let name = input.ident;
    let builder_name = Ident::new(&format!("{name}Builder"), Span::call_site());

    let Data::Struct(data_struct) = input.data else {
        unimplemented!("derive_builder is only implemented for structs, not enums or unions");
    };

    let builder_struct_decl_fields = builder_struct_decl_fields(&data_struct);
    let init_builder_struct_fields = init_builder_struct_fields(&data_struct);
    let impl_builder_field_methods = impl_builder_field_methods(&data_struct);
    let impl_builder_build_method = impl_builder_build_method(&data_struct, &name);

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name #init_builder_struct_fields
            }
        }

        #vis struct #builder_name #builder_struct_decl_fields

        impl #builder_name {
            #impl_builder_field_methods
            #impl_builder_build_method
        }
    };

    proc_macro::TokenStream::from(expanded)
}

/// Generate the fields in the Builder struct declaration.
fn builder_struct_decl_fields(data_struct: &DataStruct) -> TokenStream {
    match data_struct.fields {
        // Named structs are basically normal structs. Basically duplicate the
        // original struct body, but wrap each field type declaration in an Option< >
        Fields::Named(ref fields) => {
            let retyped_fields = fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                quote_spanned! {f.span() => #name: Option<#ty>}
            });
            quote! {
                {
                    #(#retyped_fields),*
                }
            }
        }

        // Unnamed structs are basically Rust's tuple structs, where fields are
        // numbered and not named. Create enough Option<Type> fields for how
        // many fields there are, seperated with commas, and surrounded by ();
        Fields::Unnamed(ref fields) => {
            let retyped_fields = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span() => Option<#ty>}
            });
            quote! {
                ( #(#retyped_fields),* );
            }
        }

        // Unit structs have an empty body, and need to be terminated with a semicolon
        Fields::Unit => quote! { ; },
    }
}

/// Syntax to initialize all fields of the builder struct. All fields are initially set to `None`.
fn init_builder_struct_fields(data_struct: &DataStruct) -> TokenStream {
    match data_struct.fields {
        Fields::Named(ref fields) => {
            let initialized_fields = fields.named.iter().map(|f| {
                let name = &f.ident;
                quote_spanned! {f.span() => #name: None}
            });
            quote! {
                {
                    #(#initialized_fields),*
                }
            }
        }

        Fields::Unnamed(ref fields) => {
            let initialized_fields = fields.unnamed.iter().map(|_| quote! { None });
            quote! {
                ( #(#initialized_fields),* )
            }
        }

        // Unit structs have an empty body
        Fields::Unit => TokenStream::new(),
    }
}

/// Implement all methods on the builder struct for updating its fields.
///
/// Should be substituted inside an `impl #builder_name { }` block.
fn impl_builder_field_methods(data_struct: &DataStruct) -> TokenStream {
    match data_struct.fields {
        // Generate methods that match the names and types of each field
        Fields::Named(ref fields) => {
            let field_methods = fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                quote_spanned! { f.span() =>
                    pub fn #name (&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            });
            quote! {
                #(#field_methods)*
            }
        }

        // Generate enumerated methods. Since rust methods can't start with a
        // number, prefix each field index with `field_` (e.g. `field_0()`, `field_32()`, ...)
        Fields::Unnamed(ref fields) => {
            let field_methods = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let name = Ident::new(&format!("field_{i}"), f.span());
                let index = Index::from(i);
                let ty = &f.ty;
                quote_spanned! { f.span() =>
                    pub fn #name (&mut self, value: #ty) -> &mut Self {
                        self.#index = Some(value);
                        self
                    }
                }
            });
            quote! {
                #(#field_methods)*
            }
        }

        // No fields, so no methods
        Fields::Unit => TokenStream::new(),
    }
}

fn impl_builder_build_method(data_struct: &DataStruct, name: &Ident) -> TokenStream {
    let builder_has_unset_fields = match data_struct.fields {
        Fields::Named(ref fields) => {
            let field_none_checks = fields.named.iter().map(|f| {
                let field_name = &f.ident;
                quote_spanned! { f.span() => self.#field_name.is_none() }
            });
            quote! { false #(|| #field_none_checks)* }
        }
        Fields::Unnamed(ref fields) => {
            let field_none_checks = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let index = Index::from(i);
                quote_spanned! { f.span() => self.#index.is_none() }
            });
            quote! { false #(|| #field_none_checks)* }
        }
        Fields::Unit => quote! { false },
    };

    let struct_output = match data_struct.fields {
        Fields::Named(ref fields) => {
            let unwrapped_fields = fields.named.iter().map(|f| {
                let field_name = &f.ident;
                quote_spanned! { f.span() =>
                    #field_name: self.#field_name.take().unwrap()
                }
            });
            quote! {
                #name { #(#unwrapped_fields),* }
            }
        }

        Fields::Unnamed(ref fields) => {
            let unwrapped_fields = fields.unnamed.iter().enumerate().map(|(i, f)| {
                let index = Index::from(i);
                quote_spanned! { f.span() =>
                    self.#index.take().unwrap()
                }
            });
            quote! {
                #name ( #(#unwrapped_fields),* )
            }
        }

        Fields::Unit => quote! {
            #name
        },
    };

    let unset_err_msg =
        format!("Could not build {name} struct, as one or more fields were left unset");

    quote! {
        pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
            if #builder_has_unset_fields {
                Err(#unset_err_msg.to_string().into())
            } else {
                Ok(#struct_output)
            }
        }
    }
}
