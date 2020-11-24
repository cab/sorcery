use sorcery::{rsx, use_state, Component, ComponentContext, Element};
use sorcery_dom::{render, ClickEvent, Html, HtmlContext};
use tracing::debug;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    tracing_wasm::set_as_global_default();
    let document = web_sys::window().unwrap().document().unwrap();
    let mut root = document.get_element_by_id("root").unwrap();

    render(document, root, &Element::component::<App>(None, (), vec![])).unwrap();

    Ok(())
}

#[derive(Debug, Clone)]
struct App {}

impl Component<Html> for App {
    type Props = ();
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized,
    {
        Self {}
    }

    fn render(
        &self,
        context: &mut ComponentContext,
        props: &Self::Props,
        children: &[Element<Html>],
    ) -> sorcery::Result<Element<Html>> {
        let (greeting, set_greeting) = use_state(context, &"hello");
        let (counter, set_counter) = use_state(context, &0);
        let g = "world";

        debug!("greeting is {:?}", greeting);

        Ok(rsx! {
            <div class="test-class" on_click={{ let greeting = greeting.to_owned(); let counter = *counter; move |e: &ClickEvent| {
                // debug!("clicked from rsx ({:?}): {:?}", g, e.native);
                if &greeting == &"hello" {
                    set_greeting("goodbye");
                } else {
                    set_greeting("hello");
                }
                set_counter(counter + 1);
            }}}>{&greeting} " (" {counter} ") "
                <span key="blue">
                    <Blue {g} />
                </span>
                "!"
            </div>
        })
    }
}

#[derive(Debug, Clone)]
struct Blue {}

impl Component<Html> for Blue {
    type Props = String;
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized,
    {
        Self {}
    }

    fn render(
        &self,
        context: &mut ComponentContext,
        props: &Self::Props,
        children: &[Element<Html>],
    ) -> sorcery::Result<Element<Html>> {
        Ok(rsx! {
            <span style="color: blue">{props}</span>
        })
    }
}
