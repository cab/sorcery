use proc_macro::TokenStream;
use quote::{quote, format_ident};
use std::collections::HashMap;
use syn::{Expr, Path, Field, Ident, Result, Token, braced, parse::{Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, parse_quote, token};

fn expand_props(builder: Expr, props: &HashMap<Ident, Expr>) -> proc_macro2::TokenStream {
    let pairs = props
        .iter()
        .map(|(k, v)| {
            let key = k.to_string();
            let setter = format_ident!("set_{}", key);
            quote! {
                builder. #setter ((#v).into());
            }
        })
        .collect::<Vec<_>>();
    quote! {
        {
            let mut builder = #builder;
            #(#pairs)*
            builder.build()
        }
    }
}

struct Input {
    primitive: Path,
    element: Element
}

impl Parse for Input {
    fn parse(input: ParseStream) -> Result<Self> {
        let primitive = input.parse::<Path>()?;
        let comma = input.parse::<Token![,]>()?;
        let element = input.parse::<Element>()?;
        Ok(Self {
            primitive,
            element
        })
    }
}

pub fn rsx_impl(tokens: TokenStream) -> TokenStream {
    let Input { primitive, element } = parse_macro_input!(tokens as Input);
    let result = element.walk(&Walker::new(
        { let primitive = primitive.clone(); move |element, children| {
            let tag = &element.tag;
            let tag_name = element.tag.to_string();
            let first_char = tag_name.chars().next().unwrap();
            let key = if let Some(key) = &element.key {
                quote! { Some(#key.into()) }
            } else {
                quote!{ None }
            };
            if first_char.is_lowercase() {
                let props = if let Some(direct) = &element.direct_props {
                    quote!{ (#direct).into() }
                } else { 
                    expand_props(parse_quote! {
                        sorcery::Element::<#primitive>::props_builder()
                    }, &element.props)
                };
                // native element
                quote! {
                    sorcery::Element::<#primitive>::native_for_name(#key, #tag_name, #props, vec![#(#children),*])?
                }
            } else {
                let props = if let Some(direct) = &element.direct_props {
                    quote!{ (#direct).into() }
                } else { 
                    expand_props(syn::parse_str("x").unwrap() ,&element.props)
                };
                quote! {
                    sorcery::Element::<#primitive>::component::<#tag>(#key, #props, vec![#(#children),*])
                }
            }
        }},
        {let primitive = primitive.clone(); move |text| {
            let inner = match text {
                Text::Lit(lit) => quote! { #lit },
                Text::Display(e) => quote! { (#e).to_string() },
            };
            quote! {
                sorcery::Element::<#primitive>::text(#inner)
            }
            }    },
    ));
    result.into()
}

#[derive(Debug)]
struct Element {
    tag: Ident,
    key: Option<syn::Expr>,
    props: HashMap<Ident, syn::Expr>,
    direct_props: Option<syn::Expr>,
    nodes: Vec<Node>,
}

// use a struct to avoid infinitely expanding Fn types
struct Walker<'a, O> {
    e: Box<dyn Fn(&'a Element, Vec<O>) -> O>,
    t: Box<dyn Fn(&'a Text) -> O>,
}

impl<'a, O> Walker<'a, O> {
    fn new(
        e: impl Fn(&'a Element, Vec<O>) -> O + 'static,
        t: impl Fn(&'a Text) -> O + 'static,
    ) -> Self {
        Self {
            e: Box::new(e),
            t: Box::new(t),
        }
    }
}

impl Element {
    fn walk<'w, O>(&'w self, walker: &Walker<'w, O>) -> O {
        let subresult = self
            .nodes
            .iter()
            .map(|n| match n {
                Node::Text(text) => (walker.t)(text),
                Node::Element(element) => element.walk(&walker),
            })
            .collect::<Vec<_>>();
        (walker.e)(self, subresult)
    }
}

#[derive(Debug)]
enum Node {
    Text(Text),
    Element(Box<Element>),
}

#[derive(Debug)]
enum Text {
    Lit(syn::LitStr),
    Display(syn::Expr),
}

impl Parse for Text {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Some(s) = input.parse::<syn::LitStr>().ok() {
            Ok(Text::Lit(s))
        } else {
            let content;
            braced!(content in input);
            let expr = content.parse::<syn::Expr>()?;
            Ok(Text::Display(expr))
        }
    }
}

impl Parse for Element {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![<]) && !input.peek2(Token![/]) {
            input.parse::<Token![<]>()?;
            let tag = input.parse::<Ident>()?;
            let mut is_closed = false;
            let mut props = HashMap::new();
            let mut direct_props = None;
            let mut key = None;
            loop {
                if input.peek(Token![>]) {
                    input.parse::<Token![>]>()?;
                    break;
                }
                if input.peek(Token![/]) {
                    input.parse::<Token![/]>()?;
                    input.parse::<Token![>]>()?;
                    is_closed = true;
                    break;
                }
                if input.peek(Ident) {
                    let prop = input.parse::<Ident>()?;
                    let prop_value = if input.peek(token::Eq) {
                        input.parse::<Token![=]>()?;
                        if input.peek(token::Brace) {
                            let content;
                            braced!(content in input);
                            let expr = content.parse::<syn::Expr>()?;
                            expr
                        } else {
                            if let Some(s) = input.parse::<syn::LitStr>().ok() {
                                syn::Expr::Lit(syn::ExprLit {
                                    attrs: vec![],
                                    lit: syn::Lit::Str(s),
                                })
                            } else {
                                let expr = input.parse::<syn::Expr>()?;
                                expr
                            }
                        }
                    } else {
                        syn::parse_str("true")?
                    };
                    if prop.to_string() == "key" {
                        key = Some(prop_value);
                    } else {
                        props.insert(prop, prop_value);
                    }
                } else {
                    let content;
                    braced!(content in input);
                    let expr = content.parse::<syn::Expr>()?;
                    direct_props = Some(expr);
                }
            }

            if is_closed {
                Ok(Element {
                    tag,
                    props,
                    key,
                    direct_props,
                    nodes: vec![],
                })
            } else {
                let mut nodes = Vec::<Node>::new();
                loop {
                    if let Some(text) = input.parse::<Text>().ok() {
                        nodes.push(Node::Text(text));
                    } else if let Some(element) = input.parse::<Element>().ok() {
                        nodes.push(Node::Element(Box::new(element)))
                    } else {
                        break;
                    }
                }
                input.parse::<Token![<]>()?;
                input.parse::<Token![/]>()?;
                let close_tag = input.parse::<syn::Ident>()?;
                if close_tag != tag {
                    return Err(input.error(&format!(
                        "closing tag `{}` does not match opening tag `{}`",
                        close_tag, tag
                    )));
                }
                input.parse::<Token![>]>()?;
                Ok(Element {
                    tag,
                    props,
                    key,
                    nodes,
                    direct_props,
                })
            }
        } else {
            Err(input.error("invalid element"))
        }
    }
}
