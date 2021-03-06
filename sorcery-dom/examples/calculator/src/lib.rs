use sorcery::component;
use sorcery_dom::{render, rsx, ClickEvent, Element, Html};
use tracing::debug;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    // tracing_wasm::set_as_global_default();
    let document = web_sys::window().unwrap().document().unwrap();
    let root = document.get_element_by_id("root").unwrap();
    render(document, root, &Element::component::<App>(None, (), vec![]))?;
    Ok(())
}

#[component(Html)]
fn App(context: &mut RenderContext, props: &(), children: &[Element]) -> sorcery::Result<Element> {
    let (greeting, set_greeting) = context.use_state(&"hello");
    let (counter, set_counter) = context.use_state(&0);
    let g = "world";

    debug!("greeting is {:?}", greeting);

    let sub = if counter % 2 == 0 {
        rsx! { <span>"even" <Blue {g} /></span> }
    } else {
        rsx! { <strong>"odd" <Blue {g} /></strong> }
    };

    let list = (0..*counter)
        .map(|i| Ok(rsx! { <li>"item " {i}</li> }))
        .collect::<sorcery::Result<Vec<_>>>()?;

    Ok(rsx! {
        <div class="test-class"><span style="user-select: none; cursor: pointer;" on_click={{ let greeting = greeting.to_owned(); let counter = *counter; move |e: &ClickEvent| {
            debug!("clicked from rsx ({:?}): {:?}", g, e.native);
            if &greeting == &"hello" {
                set_greeting("goodbye");
            } else {
                set_greeting("hello");
            }
            set_counter(counter + 1);
        }}}>{&greeting}</span> " (" {counter} " " [sub] ") "
            <span key="blue">
                <Blue {g}>
                    " -- also, " {greeting} " again"
                </Blue>
            </span>
            "!"
            <ul>
                [list]
            </ul>
        </div>
    })
}

#[component(Html)]
fn Blue(
    context: &mut RenderContext,
    props: &String,
    children: &[Element],
) -> sorcery::Result<Element> {
    let (counter, set_counter) = context.use_state(&0);
    debug!("the counter is {:?}", counter);
    Ok(rsx! {
        <span style="color: blue; user-select: none; cursor: pointer;"><span on_click={{  let counter = *counter; move |_: &ClickEvent| {
            debug!("UPDATE COUNTER TO {:?}", counter + 1);
            set_counter(counter + 1);
        }}}>{props}</span> " (" {counter} ")" [children]</span>
    })
}
