use sorcery::{component, Component, Element, RenderContext};
use sorcery_dom::{render, rsx, ClickEvent, Html};
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

#[component(Html)]
fn app(
    context: &mut RenderContext,
    props: &(),
    children: &[Element<Html>],
) -> sorcery::Result<Element<Html>> {
    let (greeting, set_greeting) = context.use_state(&"hello");
    let (counter, set_counter) = context.use_state(&0);
    let g = "world";

    debug!("greeting is {:?}", greeting);

    Ok(rsx! {
        <div style="user-select: none; cursor: pointer;" class="test-class"><span on_click={{ let greeting = greeting.to_owned(); let counter = *counter; move |e: &ClickEvent| {
            // debug!("clicked from rsx ({:?}): {:?}", g, e.native);
            // if &greeting == &"hello" {
            //     set_greeting("goodbye");
            // } else {
            //     set_greeting("hello");
            // }
            set_counter(counter + 1);
        }}}>{&greeting}</span> " (" {counter} ") "
            <span key="blue">
                <Blue {g} />
            </span>
            "!"
        </div>
    })
}

#[component(Html)]
fn blue(
    context: &mut RenderContext,
    props: &String,
    children: &[Element<Html>],
) -> sorcery::Result<Element<Html>> {
    let (counter, set_counter) = context.use_state(&0);
    debug!("the counter is {:?}", counter);
    Ok(rsx! {
        <span style="color: blue; user-select: none; cursor: pointer;"><span on_click={{  let counter = *counter; move |e: &ClickEvent| {
            debug!("UPDATE COUNTER TO {:?}", counter + 1);
            set_counter(counter + 1);
        }}}>{props}</span> " (" {counter} ")"</span>
    })
}
