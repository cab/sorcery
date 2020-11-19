use generational_arena::{Arena, Index as ArenaIndex};
use sorcery::{Element, RenderPrimitive};
use sorcery_reconciler::{Reconciler, Renderer};
use tracing::{debug, trace, warn};
use wasm_bindgen::prelude::*;
use web_sys::{Document, HtmlElement};

#[derive(thiserror::Error, Debug)]
enum Error {}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
enum Html {
    Div,
}

impl RenderPrimitive for Html {
    type Props = ();
    fn render(
        &self,
        props: &Self::Props,
        children: &[Element<Self>],
    ) -> sorcery::Result<Vec<Element<Self>>> {
        Ok(vec![])
    }
}

struct DomRenderer {
    nodes: Arena<HtmlElement>,
    document: Document,
}

impl Renderer<Html> for DomRenderer {
    type Container = HtmlElement;
    type InstanceKey = ArenaIndex;
    type Error = Error;

    fn create_instance(&mut self, ty: &Html, props: &()) -> Result<Self::InstanceKey> {
        let element = self.document.create_element("div")?;
        let id = self.nodes.insert(element);
        Ok(id)
    }

    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: Self::InstanceKey,
    ) -> Result<()> {
        Ok(())
    }

    fn append_child_to_parent<'r>(
        &mut self,
        parent: Self::InstanceKey,
        child: Self::InstanceKey,
    ) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn it_creates_element() {}
}
