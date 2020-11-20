use sorcery::{rsx, Component, ComponentContext, Element};
use sorcery_dom::{render, Html};
use tracing::debug;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    tracing_wasm::set_as_global_default();
    let document = web_sys::window().unwrap().document().unwrap();
    let mut root = document.get_element_by_id("root").unwrap();

    render(
        document,
        &mut root,
        &Element::component::<App>(None, (), vec![]),
    )
    .unwrap();

    Ok(())
}

struct App {}

impl Component<Html> for App {
    type Props = ();
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized,
    {
        App {}
    }

    fn render(
        &self,
        context: &mut ComponentContext,
        props: &Self::Props,
        children: &[Element<Html>],
    ) -> sorcery::Result<Element<Html>> {
        let g = "world";
        rsx! {
            <div class="test-class">"hello " {g} "!" </div>
        }
    }
}
