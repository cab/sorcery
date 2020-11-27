extern crate proc_macro;
use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Expr, GenericArgument, Generics,
    Ident, Index, ItemFn, Lit, Meta, Path, PathArguments, Signature, Type, Visibility,
};

#[derive(Debug, FromMeta)]
struct ComponentAttr {
    #[darling(default)]
    element: Option<Path>,
}

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("invalid attribute configuration: {0}")]
    InvalidConfig(#[source] darling::Error),
    #[error("invalid prop type")]
    InvalidPropType,
    #[error("invalid generic parameter")]
    InvalidGeneric,
    #[error("invalid element type: {0:?}")]
    InvalidElementType(Type),
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

pub fn component_impl(args: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemFn);
    let attr = if args.is_empty() {
        Ok(ComponentAttr { element: None })
    } else {
        let attr_args = parse_macro_input!(args as Path);
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
    element_type: Option<Type>,
    element_generic: Option<Ident>,
    render: syn::Block,
}

fn unwrap_element(ty: &Type) -> Option<syn::Type> {
    match ty {
        Type::Path(typepath) if typepath.qself.is_none() => {
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

        let inferred_element_type = item
            .sig
            .inputs
            .iter()
            .nth(2)
            .map(|arg| match arg {
                syn::FnArg::Typed(ty) => match ty.ty.as_ref() {
                    syn::Type::Reference(syn::TypeReference { elem, .. }) => match elem.as_ref() {
                        Type::Slice(syn::TypeSlice { elem, .. }) => {
                            unwrap_element(&elem).ok_or(Error::InvalidElementType(*elem.clone()))
                        }
                        other => Err(Error::InvalidElementType(other.clone())),
                    },
                    other => Err(Error::InvalidElementType(other.clone())),
                },
                _ => Err(Error::InvalidPropType),
            })
            .transpose()?;

        Ok(Config {
            element_type: attr
                .element
                .map(|path| syn::Type::Path(syn::TypePath { qself: None, path }))
                .or(inferred_element_type),
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
            //   fn #fn_name(
            //     context: &mut sorcery::RenderContext,
            //     props: &<#name as sorcery::Component<#element_type>>::Props,
            //     children: Vec<sorcery::Element<#element_type>>,
            //   ) -> sorcery::Result<sorcery::Element<#element_type>> {
            //       #name::create_element(context, props, children)
            //   }

            //   fn #fn_name_with_key(
            //       context: &mut sorcery::RenderContext,
            //       key: impl Into<sorcery::Key>,
            //       props: &<#name as Component<#element_type>>::Props,
            //       children: Vec<sorcery::Element<#element_type>>,
            //   ) -> sorcery::Result<sorcery::Element<#element_type>> {
            //       #name::create_element_with_key(context, key, props, children)
            //   }

              #[derive(Copy, Clone, Debug)]
              struct #name {}

              impl sorcery::Component<#element_type> for #name {
                  type Props = #prop_type;

                  fn new(props: &Self::Props) -> Self {
                    Self {}
                  }

                  fn render(
                      &self,
                      context: &mut sorcery::RenderContext,
                      props: &Self::Props,
                      _: &[sorcery::Element<#element_type>],
                  ) -> ::sorcery::Result<sorcery::Element<#element_type>> {
                      (#render).map(|i| i.into())
                  }
              }
            }
        } else if let Some(generic) = self.element_generic.as_ref() {
            panic!("todo?");
        } else {
            panic!("must have a generic parameter <E> or specify element type");
        };
        Ok(result)
    }
}
