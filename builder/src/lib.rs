use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Field, Fields, GenericArgument, Ident, Index, LitStr, PathArguments, PathSegment,
    Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let vis = input.vis;
    let name = input.ident;
    let builder_name = format_ident!("{name}Builder");

    let Data::Struct(data_struct) = input.data else {
        unimplemented!("derive_builder is only implemented for structs, not enums or unions");
    };

    let builder_fields = match BuilderField::from_data_struct(&data_struct) {
        Ok(builder_fields) => builder_fields,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };

    let builder_struct_decl_fields = builder_struct_decl_fields(&data_struct, &builder_fields);
    let init_builder_struct_fields = init_builder_struct_fields(&data_struct, &builder_fields);
    let impl_builder_field_methods = impl_builder_field_methods(&builder_fields);
    let impl_builder_build_method = impl_builder_build_method(&data_struct, &builder_fields, &name);

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
fn builder_struct_decl_fields(
    data_struct: &DataStruct,
    builder_fields: &[BuilderField<'_>],
) -> TokenStream {
    match data_struct.fields {
        // Named structs are basically normal structs. Basically duplicate the
        // original struct body, but wrap each field type declaration in an Option< >
        // (unless if it's already wrapped in an Option< >)
        Fields::Named(_) => {
            let retyped_fields = builder_fields.iter().map(|f| {
                let name = &f.orig_name;
                let ty = f.ty;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span => #name: #ty }
                } else {
                    quote_spanned! { f.orig_span => #name: Option<#ty> }
                }
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
        Fields::Unnamed(_) => {
            let retyped_fields = builder_fields.iter().map(|f| {
                let ty = f.ty;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span => #ty }
                } else {
                    quote_spanned! { f.orig_span => Option<#ty> }
                }
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
///
/// If a field must have an `each` builder method, then the field is instead
/// initialized to `Some(Vec::new())`.
fn init_builder_struct_fields(
    data_struct: &DataStruct,
    builder_fields: &[BuilderField<'_>],
) -> TokenStream {
    match data_struct.fields {
        Fields::Named(_) => {
            let initialized_fields = builder_fields.iter().map(|f| {
                let name = &f.orig_name;
                if f.meta.needs_each_fn() {
                    quote_spanned! { f.orig_span => #name: Some(Vec::new()) }
                } else {
                    quote_spanned! { f.orig_span => #name: None }
                }
            });
            quote! {
                {
                    #(#initialized_fields),*
                }
            }
        }

        Fields::Unnamed(_) => {
            let initialized_fields = builder_fields.iter().map(|f| {
                if f.meta.needs_each_fn() {
                    quote_spanned! { f.orig_span => Some(Vec::new()) }
                } else {
                    quote_spanned! { f.orig_span => None }
                }
            });
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
fn impl_builder_field_methods(builder_fields: &[BuilderField<'_>]) -> TokenStream {
    let field_methods = builder_fields.iter().map(|f| {
        let fn_name = &f.fn_name;
        let orig_name = &f.orig_name;
        let ty = f.optional_field_type.unwrap_or(f.ty);

        let mut builder_function = quote_spanned! { f.orig_span =>
            pub fn #fn_name (&mut self, #fn_name: #ty) -> &mut Self {
                self.#orig_name = Some(#fn_name);
                self
            }
        };

        let each_function = if f.meta.needs_each_fn() {
            // If the user requests to generate an each function with the same name
            // as the normal builder function, don't generate the normal builder
            // function.
            let each_fn_opts = &f.meta.each_fn_opts.as_ref().unwrap();
            let each_fn_name = &each_fn_opts.each_fn_name;
            let each_fn_ty = &each_fn_opts.each_fn_ty;
            let each_fn_name = quote! { #each_fn_name };
            if fn_name.to_string() == each_fn_name.to_string() {
                builder_function = TokenStream::new();
            }

            quote_spanned! { f.orig_span =>
                pub fn #each_fn_name (&mut self, #each_fn_name: #each_fn_ty) -> &mut Self {
                    // This unwrap is ok because self.#orig_name is guaranteed to be
                    // initialized to Some(Vec::new()) if an each function is
                    // generated. See the implementation of `init_builder_struct_fields()`.
                    self.#orig_name.as_mut().unwrap().push(#each_fn_name);
                    self
                }
            }
        } else {
            TokenStream::new()
        };

        quote_spanned! { f.orig_span =>
            #builder_function
            #each_function
        }
    });

    quote! {
        #(#field_methods)*
    }
}

/// Implement the `build()` method, which turns the builder struct into the actual
/// struct after checking that all fields are properly set.
fn impl_builder_build_method(
    data_struct: &DataStruct,
    builder_fields: &[BuilderField<'_>],
    output_struct_name: &Ident,
) -> TokenStream {
    let builder_has_unset_fields = match data_struct.fields {
        Fields::Named(_) => {
            let field_none_checks = builder_fields.iter().map(|f| {
                let orig_name = &f.orig_name;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span => false }
                } else {
                    quote_spanned! { f.orig_span => self.#orig_name.is_none() }
                }
            });
            quote! { false #(|| #field_none_checks)* }
        }
        Fields::Unnamed(_) => {
            let field_none_checks = builder_fields.iter().map(|f| {
                let orig_name = &f.orig_name;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span => false }
                } else {
                    quote_spanned! { f.orig_span => self.#orig_name.is_none() }
                }
            });
            quote! { false #(|| #field_none_checks)* }
        }
        Fields::Unit => quote! { false },
    };

    let struct_output = match data_struct.fields {
        Fields::Named(_) => {
            let unwrapped_fields = builder_fields.iter().map(|f| {
                let orig_name = &f.orig_name;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span =>
                        #orig_name: self.#orig_name.take()
                    }
                } else {
                    quote_spanned! { f.orig_span =>
                        #orig_name: self.#orig_name.take().unwrap()
                    }
                }
            });
            quote! {
                #output_struct_name { #(#unwrapped_fields),* }
            }
        }

        Fields::Unnamed(_) => {
            let unwrapped_fields = builder_fields.iter().map(|f| {
                let orig_name = &f.orig_name;
                if f.is_optional_field() {
                    quote_spanned! { f.orig_span => self.#orig_name.take() }
                } else {
                    quote_spanned! { f.orig_span => self.#orig_name.take().unwrap() }
                }
            });
            quote! {
                #output_struct_name ( #(#unwrapped_fields),* )
            }
        }

        Fields::Unit => quote! {
            #output_struct_name
        },
    };

    let unset_err_msg = format!(
        "Could not build {output_struct_name} struct, as one or more required fields were left unset"
    );

    quote! {
        pub fn build(&mut self) -> Result<#output_struct_name, Box<dyn std::error::Error>> {
            if #builder_has_unset_fields {
                Err(#unset_err_msg.to_string().into())
            } else {
                Ok(#struct_output)
            }
        }
    }
}

/// Gets the simple generic type `A` and inner type `T` when written literally
/// as `A<T>`.
fn get_simple_generic_and_inner_types(ty: &Type) -> Option<(&PathSegment, &Type)> {
    let Type::Path(TypePath { qself: None, path }) = ty else {
        return None;
    };

    if path.segments.len() != 1 {
        return None;
    }
    let path_seg = path.segments.first().unwrap();

    let PathSegment {
        ident: _,
        arguments: PathArguments::AngleBracketed(
            AngleBracketedGenericArguments {
                args,
                ..
            }
        )
    } = path_seg else {
        return None;
    };

    if args.len() != 1 {
        return None;
    }
    if let Some(GenericArgument::Type(inner_type)) = args.first() {
        Some((path_seg, inner_type))
    } else {
        None
    }
}

/// Gets the type T for a type written literally as `Option<T>`.
fn get_optional_type(ty: &Type) -> Option<&Type> {
    let (path_seg, inner_type) = get_simple_generic_and_inner_types(ty)?;
    if path_seg.ident != "Option" {
        return None;
    }

    Some(inner_type)
}

/// Gets the type T for a type written literally as `Vec<T>`.
fn get_vec_inner_type(ty: &Type) -> Option<&Type> {
    let (path_seg, inner_type) = get_simple_generic_and_inner_types(ty)?;
    if path_seg.ident != "Vec" {
        return None;
    }

    Some(inner_type)
}

pub(crate) struct BuilderField<'a> {
    pub(crate) orig_name: TokenStream,
    pub(crate) fn_name: TokenStream,
    pub(crate) orig_span: Span,
    pub(crate) ty: &'a Type,
    pub(crate) optional_field_type: Option<&'a Type>,
    pub(crate) meta: FieldMeta,
}

impl<'a> BuilderField<'a> {
    pub(crate) fn from_data_struct(data_struct: &'a DataStruct) -> syn::Result<Vec<Self>> {
        match &data_struct.fields {
            Fields::Named(fields) => fields
                .named
                .iter()
                .map(Self::from_named_syn_field)
                .collect(),

            Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| Self::from_unnamed_syn_field(f, i))
                .collect(),

            // Unit structs hold no fields
            Fields::Unit => Ok(Vec::new()),
        }
    }

    pub(crate) fn from_named_syn_field(syn_field: &'a syn::Field) -> syn::Result<Self> {
        let orig_name = syn_field.ident.clone().unwrap();
        let orig_name = quote_spanned! { syn_field.span() => #orig_name };
        Self::from_syn_field_with_names(syn_field, orig_name.clone(), orig_name)
    }

    pub(crate) fn from_unnamed_syn_field(
        syn_field: &'a syn::Field,
        field_number: usize,
    ) -> syn::Result<Self> {
        let orig_name = Index::from(field_number);
        let orig_name = quote_spanned! { syn_field.span() => #orig_name };

        let fn_name = format_ident!("field_{field_number}");
        let fn_name = quote_spanned! { syn_field.span() => #fn_name };

        Self::from_syn_field_with_names(syn_field, orig_name, fn_name)
    }

    fn from_syn_field_with_names(
        syn_field: &'a syn::Field,
        orig_name: TokenStream,
        fn_name: TokenStream,
    ) -> syn::Result<Self> {
        let orig_span = syn_field.span();
        let meta = FieldMeta::from_field_builder_attr(syn_field)?;
        let ty = &syn_field.ty;
        let optional_field_type = get_optional_type(ty);

        Ok(Self {
            orig_name,
            fn_name,
            orig_span,
            ty,
            optional_field_type,
            meta,
        })
    }

    pub(crate) fn is_optional_field(&self) -> bool {
        self.optional_field_type.is_some()
    }
}

#[derive(Default)]
pub(crate) struct FieldMeta {
    /// If a field has an attribute like `#[builder(each = "fn_name")]`, this is
    /// the `"fn_name"` string that should be used to create a per-item builder
    /// addition function.
    ///
    /// It can be assumed that any field tagged with the
    /// `#[builder(each = ...)]` attribute is of a type that has a `Vec`-like
    /// `.push(item)` method.
    pub(crate) each_fn_opts: Option<BuilderEachFnOpts>,
}

pub(crate) struct BuilderEachFnOpts {
    each_fn_name: Ident,
    each_fn_ty: Type,
}

impl FieldMeta {
    /// Validates and parses anything inside the `#[builder(...)]` field attribute.
    pub(crate) fn from_field_builder_attr(field: &Field) -> syn::Result<Self> {
        let mut field_meta = Self::default();

        for attr in &field.attrs {
            if attr.path().is_ident("builder") {
                attr.parse_nested_meta(|meta| {
                    let path = &meta.path;

                    if path.is_ident("each") {
                        let val = meta.value()?;
                        let val_str: LitStr = val.parse()?;
                        let val_ident: Ident = val_str.parse()?;
                        let Some(each_fn_type) = get_vec_inner_type(&field.ty) else {
                            return Err(meta.error("each functions can only be generated for vector fields where the type is written literally as `Vec<...>`"));
                        };
                        field_meta.each_fn_opts = Some(BuilderEachFnOpts {
                            each_fn_name: val_ident,
                            each_fn_ty: each_fn_type.clone(),
                        });
                    } else {
                        return Err(meta.error("unsupported builder attribute property"));
                    }

                    Ok(())
                })?;
            }
        }

        Ok(field_meta)
    }

    /// Returns true if this field should have an `each` builder function generated
    pub(crate) fn needs_each_fn(&self) -> bool {
        self.each_fn_opts.is_some()
    }
}
