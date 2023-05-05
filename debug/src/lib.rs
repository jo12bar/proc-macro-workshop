use quote::quote;
use syn::{parse_macro_input, parse_quote, spanned::Spanned, Field, Ident};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    let struct_name = &input.ident;
    let (impl_generics, impl_ty_generics, impl_where_clause) = input.generics.split_for_impl();

    let fields = match extract_all_field_info(&input) {
        Ok(ofs) => ofs,
        Err(e) => {
            let e = e.into_compile_error();
            return quote! { #e }.into();
        }
    };

    let fmt_arg: Ident = parse_quote! { fmt };

    let debug_fmt_fn_body = generate_debug_fmt_fn_body(struct_name, &fields, &fmt_arg);

    let expanded_output = quote! {
        impl #impl_generics ::core::fmt::Debug for #struct_name #impl_ty_generics
        #impl_where_clause
        {
            fn fmt(&self, #fmt_arg: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                #debug_fmt_fn_body
            }
        }
    };

    expanded_output.into()
}

/// Generates the body of the [`core::fmt::Debug::fmt()`] implementation function.
fn generate_debug_fmt_fn_body(
    struct_name: &Ident,
    fields: &[FieldInfo<'_>],
    fmt_arg: &Ident,
) -> proc_macro2::TokenStream {
    let struct_name_str = struct_name.to_string();

    let debug_struct_field_calls = fields.iter().map(|f| {
        let name = &f.name;
        let name_str = name.to_string();

        quote! {
            .field(#name_str, &self.#name)
        }
    });

    quote! {
        #fmt_arg.debug_struct(#struct_name_str)
            #(#debug_struct_field_calls)*
            .finish()
    }
}

/// A utility struct for easier access to information about one of the target struct's fields.
struct FieldInfo<'f> {
    name: &'f Ident,

    /// The base [`syn::Field`] in case if more ad-hoc inspection needs to be done
    /// and it doesn't make sense to offload the work to [`FieldInfo::new()`].
    ///
    /// If accessing anything using this field, you'll probably want to make sure
    /// to validate it somehow.
    #[allow(dead_code)]
    field: &'f Field,
}

impl<'f> FieldInfo<'f> {
    fn new(field: &'f Field) -> syn::Result<Self> {
        let Some(name) = &field.ident else {
            return Err(syn::Error::new(
                field.span(),
                "expected a named struct field, but found an unnamed field instead"
            ));
        };

        Ok(Self { name, field })
    }
}

/// Pre-extract a bunch of information about the target struct's fields into a
/// vector of [`FieldInfo`]s, allowing for easier access to information later
/// down the line (and less repeated validation).
fn extract_all_field_info(input: &syn::DeriveInput) -> syn::Result<Vec<FieldInfo<'_>>> {
    let syn::Data::Struct(data) = &input.data else {
        return Err(syn::Error::new(
            input.ident.span(),
            "enums and unions are unsupported by the derive(CustomDebug) macro; use \
             a struct with named fields instead",
        ));
    };

    let syn::Fields::Named(fields) = &data.fields else {
        return Err(syn::Error::new(
            input.ident.span(),
            "tuple structs and unit structs are unsupported by the derive(CustomDebug) macro; use \
             a struct with named fields instead",
        ));
    };

    let mut field_infos = Vec::with_capacity(fields.named.len());
    let mut collect_errors: Option<syn::Error> = None;

    for field_info in fields.named.iter().map(FieldInfo::new) {
        match field_info {
            Ok(field_info) => field_infos.push(field_info),
            Err(e) => {
                if let Some(collect_errors) = &mut collect_errors {
                    collect_errors.combine(e);
                } else {
                    collect_errors = Some(e);
                }
            }
        }
    }

    if let Some(collected_errors) = collect_errors {
        Err(collected_errors)
    } else {
        Ok(field_infos)
    }
}
