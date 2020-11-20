extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, DeriveInput, Expr, Field, Ident, Result, Token,
};

#[proc_macro_derive(Props)]
pub fn derive_props(tokens: TokenStream) -> TokenStream {
    let PropsInput { name } = parse_macro_input!(tokens as PropsInput);
    let builder_name = format_ident!("{}Builder", name);
    let builder = quote! {
        struct #builder_name {

        }
    };
    let result = quote! {
        #builder

        impl ::sorcery::Props for #name {
            type Builder = #builder_name;

            fn builder() -> Self::Builder {
                #builder_name {}
            }

        }
    };
    result.into()
}

struct PropsInput {
    name: Ident,
}

impl Parse for PropsInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let input = input.parse::<DeriveInput>()?;
        Ok(PropsInput { name: input.ident })
    }
}
