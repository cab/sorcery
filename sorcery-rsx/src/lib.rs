extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, Field, Ident, Result, Token,
};

#[proc_macro]
pub fn rsx(tokens: TokenStream) -> TokenStream {
    let element = parse_macro_input!(tokens as Element);
    let result = element.walk(&Walker::new(
        |element, children| {
            let tag = &element.tag;
            let tag_name = element.tag.to_string();
            let first_char = tag_name.chars().next().unwrap();
            if first_char.is_lowercase() {
                // native element
                quote! {
                    sorcery::Element::native_for_name(None, #tag_name, (), vec![#(#children),*])?
                }
            } else {
                quote! {
                    sorcery::Element::component::<#tag>(None, (), vec![#(#children),*])
                }
            }
        },
        |text| {
            let inner = match text {
                Text::Lit(lit) => quote! { #lit },
                Text::Display(e) => quote! { (#e).to_string() },
            };
            quote! {
                sorcery::Element::text(#inner)
            }
        },
    ));
    result.into()
}

#[derive(Debug)]
struct Element {
    tag: Ident,
    props: (),
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
                println!("prop {:?} = {:?}", prop, prop_value);
            }

            if is_closed {
                Ok(Element {
                    tag,
                    props: (),
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
                    props: (),
                    nodes,
                })
            }
        } else {
            Err(input.error("invalid element"))
        }
    }
}
