extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, DeriveInput, Expr, Field, GenericArgument, Ident, Path, PathArguments, Result, Token,
    Type, Visibility,
};

fn unwrap_option(ty: &Type) -> Option<syn::Type> {
    fn path_is_option(path: &Path) -> bool {
        path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments.iter().next().unwrap().ident == "Option"
    }

    match ty {
        Type::Path(typepath) if typepath.qself.is_none() && path_is_option(&typepath.path) => {
            // Get the first segment of the path (there is only one, in fact: "Option"):
            let type_params = &typepath.path.segments.iter().next().unwrap().arguments;
            // It should have only on angle-bracketed param ("<String>"):
            let generic_arg = match type_params {
                PathArguments::AngleBracketed(params) => params.args.iter().next().unwrap(),
                _ => panic!("TODO: error handling"),
            };
            // This argument must be a type:
            match generic_arg {
                GenericArgument::Type(ty) => Some(ty.clone()),
                _ => None,
            }
        }
        _ => panic!("TODO: error handling"),
    }
}

#[proc_macro_derive(Props)]
pub fn derive_props(tokens: TokenStream) -> TokenStream {
    let PropsInput { name, vis, fields } = parse_macro_input!(tokens as PropsInput);
    let builder_name = format_ident!("{}Builder", name);
    let field_names = fields
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect::<Vec<_>>();
    let builder_fields = fields
        .iter()
        .map(|f| {
            let name = f.ident.as_ref().unwrap();
            let ty = &f.ty;
            quote! {
                #name : ::std::option::Option<#ty>
            }
        })
        .collect::<Vec<_>>();
    let field_fns = fields
        .iter()
        .map(|f| {
            let name = f.ident.as_ref().unwrap();
            let setter_name = format_ident!("set_{}", name);
            let ty = &f.ty;
            if let Some(unwrapped) = unwrap_option(ty) {
                quote! {
                    pub fn #setter_name (&mut self, value: #unwrapped) {
                        self.#name = Some(Some(value));
                    }
                }
            } else {
                quote! {
                    pub fn #setter_name (&mut self, value: #ty) {

                    }
                }
            }
        })
        .collect::<Vec<_>>();
    let builder = quote! {
        #vis struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {

            #(#field_fns)*

            pub fn build(self) -> #name {
                #name {
                    #( #field_names: self.#field_names.unwrap_or(None)),*
                }
            }
        }

    };
    let result = quote! {
        #builder

        impl ::sorcery::Props for #name {
            type Builder = #builder_name;

            fn builder() -> Self::Builder {
                #builder_name {
                    #( #field_names: None ),*
                }
            }

        }
    };
    result.into()
}

struct PropsInput {
    name: Ident,
    vis: Visibility,
    fields: Vec<Field>,
}

impl Parse for PropsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let input = input.parse::<DeriveInput>()?;
        let fields = match input.data {
            syn::Data::Struct(syn::DataStruct {
                fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
                ..
            }) => named.into_iter().collect(),
            _ => unimplemented!("props must be a struct with named fields"),
        };
        Ok(PropsInput {
            name: input.ident,
            vis: input.vis,
            fields,
        })
    }
}
