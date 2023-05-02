use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Ident};

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

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name #init_builder_struct_fields
            }
        }

        #vis struct #builder_name #builder_struct_decl_fields
    };

    proc_macro::TokenStream::from(expanded)
}

/// Generate the fields in the Builder struct declaration.
fn builder_struct_decl_fields(data_struct: &DataStruct) -> TokenStream {
    match data_struct.fields {
        // Named structs are basically normal structs. Basically duplicate the
        // original struct body, but wrap each field type declaration in an Option< >
        syn::Fields::Named(ref fields) => {
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
        syn::Fields::Unnamed(ref fields) => {
            let retyped_fields = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span() => Option<#ty>}
            });
            quote! {
                ( #(#retyped_fields),* );
            }
        }

        // Unit structs have an empty body, and need to be terminated with a semicolon
        syn::Fields::Unit => quote! { ; },
    }
}

/// Syntax to initialize all fields of the builder struct. All fields are initially set to `None`.
fn init_builder_struct_fields(data_struct: &DataStruct) -> TokenStream {
    match data_struct.fields {
        syn::Fields::Named(ref fields) => {
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

        syn::Fields::Unnamed(ref fields) => {
            let initialized_fields = fields.unnamed.iter().map(|_| quote! { None });
            quote! {
                ( #(#initialized_fields),* )
            }
        }

        // Unit structs have an empty body
        syn::Fields::Unit => TokenStream::new(),
    }
}
