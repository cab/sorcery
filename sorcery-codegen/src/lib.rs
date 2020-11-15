extern crate proc_macro;
use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Expr, GenericArgument, Generics,
    Ident, Index, ItemFn, Lit, Meta, PathArguments, Signature, Type, Visibility,
};

#[derive(Debug, FromMeta)]
struct ComponentAttr {
    #[darling(default)]
    element: Option<Ident>,
}

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("invalid attribute configuration: {0}")]
    InvalidConfig(#[source] darling::Error),
    #[error("invalid prop type")]
    InvalidPropType,
    #[error("invalid generic parameter")]
    InvalidGeneric,
}

impl Error {
    fn span(&self) -> Span {
        match self {
            _ => Span::call_site(),
        }
    }

    fn emit(&self) -> proc_macro2::TokenStream {
        let message = format!("{}", self);
        quote_spanned!(self.span() => compile_error!(#message);)
    }
}

#[proc_macro_attribute]
pub fn component(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemFn);
    let attr = if args.is_empty() {
        Ok(ComponentAttr { element: None })
    } else {
        let attr_args = parse_macro_input!(args as Ident);
        Ok(ComponentAttr {
            element: Some(attr_args),
        })
    };

    let result = attr
        .and_then(|attr| Config::parse(attr, &mut input))
        .and_then(|mut config| config.generate());

    let comp = match result {
        Ok(ctor) => ctor,
        Err(error) => error.emit(),
    };

    let output = quote! {
        #comp
    };

    TokenStream::from(output)
}

struct Config {
    name: Ident,
    prop_type: Option<Type>,
    element_type: Option<Ident>,
    element_generic: Option<Ident>,
    render: syn::Block,
}

impl Config {
    fn parse(attr: ComponentAttr, item: &mut ItemFn) -> Result<Self, Error> {
        let name = item.sig.ident.clone();
        let prop_type = item
            .sig
            .inputs
            .iter()
            .nth(1)
            .map(|arg| match arg {
                syn::FnArg::Typed(ty) => match ty.ty.as_ref() {
                    syn::Type::Reference(ty) => Ok(*(ty.elem.clone())),
                    _ => Err(Error::InvalidPropType),
                },
                _ => Err(Error::InvalidPropType),
            })
            .transpose()?;
        let render = *(item.block.clone());
        let element_generic = item
            .sig
            .generics
            .params
            .first()
            .map(|p| match p {
                syn::GenericParam::Type(tp) => Ok(tp.ident.clone()),
                _ => Err(Error::InvalidGeneric),
            })
            .transpose()?;
        Ok(Config {
            element_type: attr.element,
            element_generic,
            name,
            prop_type,
            render,
        })
    }

    fn generate(&mut self) -> Result<proc_macro2::TokenStream, Error> {
        use heck::CamelCase;
        let name = format_ident!("{}", self.name.to_string().to_camel_case());
        let fn_name = format_ident!("{}", self.name);
        let fn_name_with_key = format_ident!("{}_with_key", fn_name);
        let prop_type = self.prop_type.as_ref().unwrap(); // unwrap_or(UNIT);
        let render = &self.render;
        let result = if let Some(element_type) = self.element_type.as_ref() {
            quote! {
              fn #fn_name(
                context: &mut sorcery::ComponentContext,
                props: &<#name as sorcery::Component<TestElement>>::Props,
                children: Vec<sorcery::Element<#element_type>>,
              ) -> sorcery::Result<sorcery::Element<#element_type>> {
                  #name::create_element(context, props, children)
              }

              fn #fn_name_with_key(
                  context: &mut sorcery::ComponentContext,
                  key: impl Into<sorcery::Key>,
                  props: &<#name as Component<TestElement>>::Props,
                  children: Vec<sorcery::Element<#element_type>>,
              ) -> sorcery::Result<sorcery::Element<#element_type>> {
                  #name::create_element_with_key(context, key, props, children)
              }

              struct #name {}

              impl sorcery::Component<#element_type> for #name {
                  type Props = #prop_type;

                  fn new(props: &Self::Props) -> Self {
                    Self {}
                  }

                  fn render(
                      &self,
                      context: &mut sorcery::ComponentContext,
                      props: &Self::Props,
                      _: Vec<sorcery::Element<#element_type>>,
                  ) -> Result<sorcery::Element<#element_type>> {
                      (#render).map(|i| i.into())
                  }
              }
            }
        } else if let Some(generic) = self.element_generic.as_ref() {
            quote! {
              fn #fn_name <#generic>(
                context: &mut sorcery::ComponentContext,
                props: &<#name as sorcery::Component<TestElement>>::Props,
                children: Vec<sorcery::Element<#generic>>,
              ) -> sorcery::Result<sorcery::Element<#generic>> {
                  #name::create_element(props, children)
              }

              fn #fn_name_with_key <#generic>(
                  context: &mut sorcery::ComponentContext,
                  key: impl Into<sorcery::Key>,
                  props: &<#name as Component<TestElement>>::Props,
                  children: Vec<sorcery::Element<#generic>>,
              ) -> sorcery::Result<sorcery::Element<#generic>> {
                  #name::create_element_with_key(key, props, children)
              }

              struct #name {}

              impl <#generic> sorcery::Component<#generic> for #name {
                  type Props = #prop_type;

                  fn new(props: &Self::Props) -> Self {
                      Self {}
                  }

                  fn render(
                      &self,
                      context: &mut sorcery::ComponentContext,
                      props: &Self::Props,
                      _: Vec<sorcery::Element<#generic>>,
                  ) -> Result<sorcery::Element<#generic>> {
                      (#render).map(|i| i.into())
                  }
              }
            }
        } else {
            panic!("must have a generic parameter <E> or specify element type");
        };
        Ok(result)
    }
}
