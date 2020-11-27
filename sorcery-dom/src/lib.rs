use dyn_clone::DynClone;
use generational_arena::{Arena, Index as ArenaIndex};
use gloo::{events::EventListener, timers::callback::Timeout};
use sorcery::{
    reconciler::{self, LocalTask, Reconciler, Task, TaskPriority},
    Props, RenderPrimitive, StoredProps,
};
use std::{cell::RefCell, collections::HashMap, sync::Arc};
use tracing::{debug, trace, warn};
use wasm_bindgen::{prelude::*, JsCast};
use web_sys::{Document, Element, HtmlElement, Node};

#[macro_export]
macro_rules! rsx {
    ($($element:tt)*) => {
        sorcery::rsx! { $crate::Html, $($element)* }
    };
}

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

#[derive(Debug, Clone, PartialEq)]
pub struct Html {
    tag: String,
}

#[derive(Props, Debug, Clone, PartialEq)]
pub struct HtmlProps {
    on_click: Option<Callback<ClickEvent>>,
    style: Option<String>,
    class: Option<String>,
}

#[derive(Clone)]
pub struct Callback<A>(Arc<dyn Fn(&A)>);

impl<A> PartialEq<Callback<A>> for Callback<A> {
    fn eq(&self, other: &Callback<A>) -> bool {
        // TODO(cab) is this right?
        // never equal
        false
    }
}

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
    listeners: HashMap<ArenaIndex, Vec<EventListener>>,
}

impl Renderer {
    pub fn new(document: Document) -> Self {
        Self {
            document,
            nodes: Arena::new(),
            listeners: HashMap::new(),
        }
    }
}

pub fn render(
    document: Document,
    mut container: Element,
    element: &sorcery::Element<Html>,
) -> reconciler::Result<(), Error> {
    let renderer = Renderer::new(document);
    let mut reconciler = reconciler::Reconciler::new(renderer, container);
    reconciler.render(element)?;
    wasm_bindgen_futures::spawn_local(async move {
        reconciler.run().await;
    });
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClickEvent {
    pub native: web_sys::MouseEvent,
}

impl Renderer {
    fn update_props(
        &mut self,
        instance: ArenaIndex,
        props: &HtmlProps,
    ) -> std::result::Result<(), Error> {
        self.listeners.remove(&instance);
        let element: &Element = self.nodes.get(instance).unwrap().unchecked_ref();
        if let Some(f) = &props.on_click {
            let on_click = EventListener::new(element, "click", {
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

            if !self.listeners.contains_key(&instance) {
                self.listeners.insert(instance.clone(), vec![]);
            }

            self.listeners.get_mut(&instance).unwrap().push(on_click);

            // TODO we should track this so it drops appropriately
            // on_click.forget();
            // element.add_event_listener_with_callback("click", &Closure::wrap(f))?;
        }

        if let Some(style) = &props.style {
            element.set_attribute("style", style)?;
        }

        Ok(())
    }
}

impl reconciler::Renderer<Html> for Renderer {
    type Container = Element;
    type InstanceKey = ArenaIndex;
    type TextInstanceKey = ArenaIndex;
    type Error = Error;

    fn schedule_task(
        &self,
        priority: TaskPriority,
        task: Box<dyn Task>,
    ) -> std::result::Result<(), Self::Error> {
        match priority {
            TaskPriority::Immediate => {
                wasm_bindgen_futures::spawn_local(async move {
                    task.run().await.unwrap();
                });
            }
            TaskPriority::High | TaskPriority::Idle => {
                web_sys::window()
                    .unwrap()
                    .request_idle_callback(
                        Closure::once_into_js(move || {
                            wasm_bindgen_futures::spawn_local(async move {
                                task.run().await.unwrap();
                            });
                        })
                        .unchecked_ref(),
                    )
                    .unwrap();
            }
        };
        Ok(())
    }

    fn schedule_local_task(
        &self,
        priority: TaskPriority,
        task: Box<dyn LocalTask>,
    ) -> std::result::Result<(), Self::Error> {
        match priority {
            TaskPriority::Immediate => {
                wasm_bindgen_futures::spawn_local(async move {
                    task.run().await.unwrap();
                });
            }
            TaskPriority::High | TaskPriority::Idle => {
                web_sys::window()
                    .unwrap()
                    .request_idle_callback(
                        Closure::once_into_js(move || {
                            wasm_bindgen_futures::spawn_local(async move {
                                task.run().await.unwrap();
                            });
                        })
                        .unchecked_ref(),
                    )
                    .unwrap();
            }
        };
        Ok(())
    }

    fn update_instance_props(
        &mut self,
        instance: &Self::InstanceKey,
        new_props: HtmlProps,
    ) -> std::result::Result<(), Self::Error> {
        debug!("UPDATING INSTANCE PROPS...");
        self.update_props(*instance, &new_props)?;
        Ok(())
    }

    fn create_instance(
        &mut self,
        ty: &Html,
        props: &<Html as RenderPrimitive>::Props,
        debug: &sorcery::reconciler::InstanceDebug,
    ) -> Result<Self::InstanceKey> {
        let mut element = self.document.create_element(&ty.tag)?;
        // element.set_attribute("data-sorcery-fiber-id", &debug.id)?;
        let id = self.nodes.insert(element.unchecked_into());
        self.update_props(id, &props)?;
        Ok(id)
    }

    fn update_text(
        &mut self,
        instance: &Self::TextInstanceKey,
        text: &str,
    ) -> std::result::Result<(), Self::Error> {
        debug!("UPDATE THE TEXT TO {:?}", text);
        let node = self.nodes.get_mut(*instance).unwrap();
        node.set_node_value(Some(text));
        Ok(())
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
        debug!("append text to container {:?} -> {:?}", text, parent);
        let (parent, text) = self.nodes.get2_mut(*parent, *text);
        parent.unwrap().append_child(&text.unwrap())?;
        Ok(())
    }

    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        debug!("append node to container {:?}", child);
        let node = self.nodes.get(*child).unwrap();
        container.append_child(node)?;
        Ok(())
    }

    fn append_child_to_parent<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        debug!("requested to append {:?} to {:?}", child, parent);
        let (parent, child) = self.nodes.get2_mut(*parent, *child);
        parent.unwrap().append_child(&child.unwrap())?;
        Ok(())
    }

    fn insert_child_in_parent_before<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
        before: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error> {
        debug!("requested to insert {:?} in {:?}", child, parent);
        let before = self.nodes.get(*before).unwrap().clone();
        let (parent, child) = self.nodes.get2_mut(*parent, *child);
        parent
            .unwrap()
            .insert_before(child.unwrap(), Some(&before))?;
        Ok(())
    }

    fn insert_child_in_container_before<'r>(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
        before: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error> {
        debug!("requested to insert {:?} in container", child);
        let before = self.nodes.get(*before).unwrap().clone();
        let child = self.nodes.get_mut(*child);
        container.insert_before(child.unwrap(), Some(&before))?;
        Ok(())
    }

    fn remove_child_from_parent<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        debug!("requested to remove {:?} from {:?}", child, parent);
        let (parent, child) = self.nodes.get2_mut(*parent, *child);
        parent.unwrap().remove_child(child.unwrap())?;
        Ok(())
    }

    fn remove_child_from_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> Result<()> {
        debug!("requested to remove {:?} from container", child);
        let node = self.nodes.get(*child).unwrap();
        container.remove_child(node)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn it_creates_element() {}
}
