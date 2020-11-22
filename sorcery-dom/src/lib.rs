use dyn_clone::DynClone;
use generational_arena::{Arena, Index as ArenaIndex};
use gloo::{events::EventListener, timers::callback::Timeout};
use sorcery::{Props, RenderPrimitive, StoredProps};
use sorcery_reconciler::{Reconciler, Task};
use std::{cell::RefCell, collections::HashMap, sync::Arc};
use tracing::{debug, trace, warn};
use wasm_bindgen::{prelude::*, JsCast};
use web_sys::{Document, Element, HtmlElement, Node};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("javascript error: {0:?}")]
    Js(wasm_bindgen::JsValue),
}

impl From<wasm_bindgen::JsValue> for Error {
    fn from(js: JsValue) -> Self {
        Error::Js(js)
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Html {
    tag: String,
}

#[derive(Props, Debug, Clone)]
pub struct HtmlProps {
    on_click: Option<Callback<ClickEvent>>,
    style: Option<String>,
    class: Option<String>,
}

#[derive(Clone)]
pub struct Callback<A>(Arc<dyn Fn(&A)>);

impl<A> std::fmt::Debug for Callback<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Callback").finish()
    }
}

impl<F, A> From<F> for Callback<A>
where
    F: Fn(&A) + 'static,
{
    fn from(f: F) -> Self {
        Callback(Arc::new(f))
    }
}

impl RenderPrimitive for Html {
    type Props = HtmlProps;

    fn for_name(name: &str) -> Option<Self> {
        Some(Html {
            tag: name.to_string(),
        })
    }

    fn render(
        &self,
        props: &Self::Props,
        children: &[sorcery::Element<Self>],
    ) -> sorcery::Result<Vec<sorcery::Element<Self>>> {
        Ok(children.to_vec())
    }
}

#[derive(Debug)]
struct Renderer {
    nodes: Arena<Node>,
    document: Document,
    listeners: Vec<EventListener>,
}

impl Renderer {
    pub fn new(document: Document) -> Self {
        Self {
            document,
            nodes: Arena::new(),
            listeners: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct HtmlContext {}

impl Default for HtmlContext {
    fn default() -> Self {
        Self {}
    }
}

impl sorcery_reconciler::RendererContext for HtmlContext {
    fn context_for() -> Self {
        Self {}
    }
}

pub fn render(
    document: Document,
    container: &mut Element,
    element: &sorcery::Element<Html>,
) -> sorcery_reconciler::Result<(), Error> {
    let renderer = Renderer::new(document);
    let mut reconciler = sorcery_reconciler::Reconciler::new(renderer);
    reconciler.create_container(container);
    let mut ctx = sorcery_reconciler::Context::new();
    reconciler.update_container(&mut ctx, container, element)?;
    Ok(())
}

#[derive(Debug, Clone)]
pub struct ClickEvent {
    pub native: web_sys::MouseEvent,
}

impl sorcery_reconciler::Renderer<Html> for Renderer {
    type Container = Element;
    type InstanceKey = ArenaIndex;
    type TextInstanceKey = ArenaIndex;
    type Error = Error;

    fn schedule_task(&self, task: Box<dyn Task>) {
        web_sys::window()
            .unwrap()
            .request_idle_callback(
                Closure::once_into_js(move || {
                    task.run();
                })
                .unchecked_ref(),
            )
            .unwrap();
    }

    fn create_instance(
        &mut self,
        ty: &Html,
        props: &<Html as RenderPrimitive>::Props,
    ) -> Result<Self::InstanceKey> {
        let element = self.document.create_element(&ty.tag)?;
        if let Some(f) = &props.on_click {
            let on_click = EventListener::new(&element, "click", {
                let f = f.clone();
                move |event| {
                    let event = event
                        .clone()
                        .dyn_into::<web_sys::MouseEvent>()
                        .unwrap_throw();
                    debug!("clicked!");
                    f.0(&ClickEvent { native: event });
                }
            });

            self.listeners.push(on_click);

            // TODO we should track this so it drops appropriately
            // on_click.forget();
            // element.add_event_listener_with_callback("click", &Closure::wrap(f))?;
        }
        let id = self.nodes.insert(element.unchecked_into());
        Ok(id)
    }

    fn create_text_instance(&mut self, text: &str) -> Result<Self::TextInstanceKey> {
        let element = self.document.create_text_node(text);
        let id = self.nodes.insert(element.unchecked_into());
        Ok(id)
    }

    fn append_text_to_parent(
        &mut self,
        parent: &Self::InstanceKey,
        text: &Self::TextInstanceKey,
    ) -> std::result::Result<(), Self::Error> {
        let (parent, text) = self.nodes.get2_mut(*parent, *text);
        parent.unwrap().append_child(&text.unwrap())?;
        Ok(())
    }

    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        let node = self.nodes.get(*child).unwrap();
        container.append_child(node)?;
        Ok(())
    }

    fn append_child_to_parent<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        let (parent, child) = self.nodes.get2_mut(*parent, *child);
        parent.unwrap().append_child(&child.unwrap())?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn it_creates_element() {}
}
