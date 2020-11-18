use indextree::{Arena, NodeId as ArenaNodeId};
use tracing::{debug, trace};

use crate::{
    AnyComponent, Component, ComponentElement, ComponentId, Element, Key, NativeElement,
    RenderPrimitive, Result, StoredProps, StoredState,
};
use std::{
    any::Any,
    collections::{HashMap, VecDeque},
};

pub struct Reconciler<P, R>
where
    R: Renderer<P>,
    P: RenderPrimitive,
{
    next_fiber_id: u32,
    element_type: std::marker::PhantomData<P>,
    renderer: R,
    fibers: Arena<Fiber<P, R::Instance>>,
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Hash, Eq)]
struct FiberId(u32);

#[derive(Debug)]
struct Fiber<P, R>
where
    P: RenderPrimitive,
{
    id: FiberId,
    body: Option<FiberBody<P, R>>,
    sibling: Option<ArenaNodeId>,
    child: Option<ArenaNodeId>,
    parent: Option<ArenaNodeId>,
}

impl<P, R> Fiber<P, R>
where
    P: RenderPrimitive,
{
    fn parent<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.parent
            .as_ref()
            .and_then(|p| arena.get(*p))
            .map(|n| n.get())
    }

    fn sibling<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.sibling
            .as_ref()
            .and_then(|p| arena.get(*p))
            .map(|n| n.get())
    }

    fn is_native(&self) -> bool {
        match &self.body {
            Some(FiberBody::Root) => true,
            Some(FiberBody::Native { .. }) => true,
            _ => false,
        }
    }

    fn is_root(&self) -> bool {
        match &self.body {
            Some(FiberBody::Root) => true,
            _ => false,
        }
    }

    fn native_instance(&self) -> Option<&R> {
        match &self.body {
            Some(FiberBody::Native {
                native_instance, ..
            }) => native_instance.as_ref(),
            _ => None,
        }
    }

    fn native_instance_mut(&mut self) -> Option<&mut R> {
        match &mut self.body {
            Some(FiberBody::Native {
                native_instance, ..
            }) => native_instance.as_mut(),
            _ => None,
        }
    }

    fn parent_native<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        let mut parent = self.parent(arena);
        while let Some(p) = parent {
            if p.is_native() {
                return Some(p);
            }
            parent = p.parent(arena);
        }
        None
    }

    fn parent_native_mut<'a>(&self, arena: &'a mut Arena<Self>) -> Option<&'a mut Self> {
        let id = self.parent_native_id(arena)?;
        arena.get_mut(id).map(|n| n.get_mut())
    }

    fn parent_native_id<'a>(&'a self, arena: &'a Arena<Self>) -> Option<ArenaNodeId> {
        let mut parent = self.parent;
        while let Some(p) = parent {
            if let Some(node) = arena.get(p).map(|n| n.get()) {
                if node.is_native() {
                    return Some(p);
                }
                parent = node.parent;
            }
        }
        None
    }
}

impl<P, R> PartialEq<Fiber<P, R>> for Fiber<P, R>
where
    P: RenderPrimitive,
{
    fn eq(&self, other: &Fiber<P, R>) -> bool {
        self.id == other.id
    }
}

impl<P, R> Fiber<P, R>
where
    P: RenderPrimitive,
{
    fn root(id: FiberId) -> Self {
        let body = FiberBody::Root;
        Fiber {
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(body),
        }
    }
    fn component(
        id: FiberId,
        instance: Box<dyn AnyComponent<P>>,
        props: Box<dyn StoredProps>,
    ) -> Self {
        let state = Box::new(());
        let body = FiberBody::Component {
            instance,
            state,
            props,
        };
        Self {
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(body),
        }
    }

    fn native(id: FiberId, instance: P, props: P::Props) -> Self {
        let body = FiberBody::Native {
            instance,
            props,
            native_instance: None,
        };
        let state = Box::new(());
        Self {
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(body),
        }
    }

    fn render(&mut self, children: &[Element<P>]) -> Result<Vec<Element<P>>> {
        // let child_elements = self
        //     .children
        //     .iter_mut()
        //     .map(|fiber| fiber.render())
        //     .collect::<Result<Vec<_>>>()?
        //     .into_iter()
        //     .filter_map(|f| f)
        //     .collect::<Vec<_
        match &self.body {
            Some(FiberBody::Root) => {
                unimplemented!();
            }
            Some(FiberBody::Component {
                instance, props, ..
            }) => {
                let mut context = super::Context::new();
                let mut ccontext = context.component();
                Ok(vec![instance.render(
                    &mut ccontext,
                    props.as_ref(),
                    children,
                )?])
            }
            Some(FiberBody::Native {
                instance, props, ..
            }) => Ok(instance.render(&props, children)?),
            None => {
                unimplemented!();
            }
        }
    }
}

#[derive(Debug)]
enum FiberBody<P, R>
where
    P: RenderPrimitive,
{
    Root,
    Component {
        instance: Box<dyn AnyComponent<P>>,
        state: Box<dyn StoredState>,
        props: Box<dyn StoredProps>,
    },
    Native {
        instance: P,
        native_instance: Option<R>,
        props: P::Props,
    },
}

fn process_wrap<P, R>(
    mut f: impl FnMut(&Fiber<P, R>) -> Result<()>,
) -> impl FnMut(&Fiber<P, R>) -> Result<Option<ArenaNodeId>>
where
    P: RenderPrimitive,
{
    move |fiber| {
        f(fiber)?;
        Ok(fiber.child)
    }
}

fn process_wrap_mut<P, R>(
    mut f: impl FnMut(&mut Fiber<P, R>) -> Result<()>,
) -> impl FnMut(&mut Fiber<P, R>) -> Result<Option<ArenaNodeId>>
where
    P: RenderPrimitive,
{
    move |fiber| {
        f(fiber)?;
        Ok(fiber.child)
    }
}

fn walk_fibers<P, R>(
    arena: &Arena<Fiber<P, R>>,
    start: ArenaNodeId,
    mut process: impl FnMut(&Fiber<P, R>) -> Result<Option<ArenaNodeId>>,
) -> Result<()>
where
    P: RenderPrimitive,
{
    let start = match arena.get(start) {
        Some(n) => n.get(),
        _ => return Ok(()),
    };
    let root = start;
    let mut current = start;
    loop {
        let child = process(current)?;
        if let Some(child) = child.and_then(|c| arena.get(c)) {
            current = child.get();
            continue;
        }

        if current == root {
            return Ok(());
        }

        while current.sibling.is_none() {
            if current.parent.is_none() || current.parent(arena).map(|p| p == root).unwrap_or(false)
            {
                return Ok(());
            }
            if let Some(parent) = current.parent(arena) {
                current = parent;
            }
        }

        if let Some(sibling) = current.sibling(arena) {
            current = sibling;
        }
    }
}

fn walk_fibers_mut<P, R>(
    arena: &mut Arena<Fiber<P, R>>,
    start: ArenaNodeId,
    mut process: impl FnMut(&mut Fiber<P, R>) -> Result<Option<ArenaNodeId>>,
) -> Result<()>
where
    P: RenderPrimitive,
{
    let root = start;
    let mut current = start;
    loop {
        let child = if let Some(current) = arena.get_mut(current).map(|n| n.get_mut()) {
            process(current)?
        } else {
            panic!();
        };
        if let Some(child) = child {
            current = child;
            continue;
        }

        if current == root {
            return Ok(());
        }

        loop {
            let current_node = arena.get(current).map(|n| n.get()).unwrap(); // todo
            if !current_node.sibling.is_none() {
                break;
            }
            if current_node.parent.is_none()
                || current_node.parent.map(|p| p == root).unwrap_or(false)
            {
                return Ok(());
            }
            if let Some(parent) = current_node.parent {
                current = parent;
            }
        }
        let current_node = arena.get(current).map(|n| n.get()).unwrap(); // todo

        if let Some(sibling) = current_node.sibling {
            current = sibling;
        }
    }
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    pub fn new(renderer: R) -> Self {
        Self {
            renderer,
            next_fiber_id: 0,
            fibers: Arena::new(),
            element_type: std::marker::PhantomData,
        }
    }

    pub fn create_container(&mut self, base: R::Container) -> R::Container {
        base
    }

    fn next_fiber_id(&mut self) -> FiberId {
        let id = self.next_fiber_id;
        self.next_fiber_id += 1;
        let id = FiberId(id);
        id
    }

    fn build_tree<'a>(
        &mut self,
        arena: &'a mut Arena<Fiber<P, R::Instance>>,
        element: &Element<P>,
    ) -> Result<ArenaNodeId> {
        debug!("rendering a {:?}\n\n", element);
        let children = element.children();
        let mut fiber = match element {
            Element::Component(comp_element) => {
                let instance = comp_element.construct()?;
                let fiber =
                    Fiber::component(self.next_fiber_id(), instance, comp_element.props.clone());
                Ok(fiber)
            }
            Element::Native(native) => {
                let instance = native.ty.clone();
                let fiber = Fiber::native(self.next_fiber_id(), instance, native.props.clone());
                Ok(fiber)
            }
        }?;
        let id = arena.new_node(fiber);
        {
            let to_child = {
                let mut node = arena.get_mut(id).unwrap().get_mut();
                node.render(&children)?
                    .into_iter()
                    .fold(Ok(None), |prev, next| {
                        let child_id = self.build_tree(arena, &next)?;
                        let mut child = arena.get_mut(child_id).unwrap().get_mut();
                        child.parent = Some(id.clone());
                        child.sibling = prev?;
                        Ok(Some(child_id))
                    })?
            };
            let mut node = arena.get_mut(id).unwrap().get_mut();
            node.child = to_child;
        }
        Ok(id)
    }

    fn root_fiber(&mut self) -> Fiber<P, R::Instance> {
        Fiber::root(self.next_fiber_id())
    }

    pub fn update_container(
        &mut self,
        container: &mut R::Container,
        element: &Element<P>,
    ) -> Result<()> {
        let mut arena = Arena::new();
        let root_id = {
            let node_id = self.build_tree(&mut arena, element)?;
            let mut root_fiber = self.root_fiber();
            root_fiber.child = Some(node_id);
            let root_id = arena.new_node(root_fiber);
            arena.get_mut(node_id).unwrap().get_mut().parent = Some(root_id);
            root_id
        };
        walk_fibers_mut(
            &mut arena,
            root_id,
            process_wrap_mut(|fiber| {
                match &mut fiber.body {
                    Some(FiberBody::Native {
                        ref mut native_instance,
                        instance,
                        props,
                        ..
                    }) => {
                        *native_instance = Some(self.renderer.create_instance(instance, props)?);
                    }
                    _ => {}
                };
                Ok(())
            }),
        )?;
        walk_fibers_mut(
            &mut arena,
            root_id,
            process_wrap_mut(|fiber| {
                let parent = fiber.parent_native_mut(&mut arena);
                match &fiber.body {
                    Some(FiberBody::Native {
                        native_instance: Some(native_instance),
                        ..
                    }) => {
                        if parent.as_ref().map_or(false, |p| p.is_native()) {
                            if parent.as_ref().map_or(false, |p| p.is_root()) {
                                debug!("append to container");
                                self.renderer
                                    .append_child_to_container(container, native_instance);
                            } else if let Some(parent) =
                                parent.and_then(|p| p.native_instance_mut())
                            {
                                self.renderer
                                    .append_child_to_parent(parent, native_instance);
                            }
                        }
                    }
                    _ => {}
                }
                Ok(())
            }),
        )?;
        // self.renderer.append_child_to_container(container, tree);
        Ok(())
    }
}

#[derive(Debug)]
pub enum Op {}

pub trait Renderer<P>
where
    P: RenderPrimitive,
{
    type Container;
    type Instance: Clone + std::fmt::Debug;
    fn create_instance(&mut self, ty: &P, props: &P::Props) -> Result<Self::Instance>;
    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::Instance,
    ) -> Result<()>;
    fn append_child_to_parent(
        &mut self,
        parent: &mut Self::Instance,
        child: &Self::Instance,
    ) -> Result<()>;
}

#[cfg(test)]
mod test {
    use test_env_log::test;
    mod sorcery {
        pub use super::super::*;
    }

    use super::{NativeElement, Reconciler, RenderPrimitive, Renderer};
    use crate::{component, use_state, Component, ComponentContext, Element, Result};

    struct StringRenderer {}

    impl StringRenderer {
        fn new() -> Self {
            Self {}
        }
    }

    #[derive(Debug, Clone)]
    struct Str(String);

    #[derive(Debug, Clone)]
    struct StringNode {
        value: String,
        children: Vec<StringNode>,
    }

    impl StringNode {
        fn to_string(&self) -> String {
            format!(
                "{}{}",
                self.value,
                self.children
                    .iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<_>>()
                    .join("")
            )
        }
    }

    impl RenderPrimitive for Str {
        type Props = ();
        fn render(
            &self,
            props: &Self::Props,
            children: &[Element<Self>],
        ) -> Result<Vec<Element<Self>>> {
            Ok(children.to_vec())
        }
    }

    impl Renderer<Str> for StringRenderer {
        type Container = StringNode;
        type Instance = StringNode;
        fn create_instance(&mut self, ty: &Str, props: &()) -> Result<Self::Instance> {
            Ok(StringNode {
                value: ty.0.to_owned(),
                children: vec![],
            })
        }

        fn append_child_to_parent(
            &mut self,
            container: &mut Self::Container,
            child: &Self::Instance,
        ) -> Result<()> {
            Ok(())
        }

        fn append_child_to_container(
            &mut self,
            container: &mut Self::Container,
            child: &Self::Instance,
        ) -> Result<()> {
            container.children.push(child.clone());
            Ok(())
        }
    }

    struct List {}

    impl Component<Str> for List {
        type Props = ();

        fn name(&self) -> String {
            "List".to_string()
        }

        fn new(props: &Self::Props) -> Self {
            Self {}
        }

        fn render(
            &self,
            context: &mut ComponentContext,
            props: &Self::Props,
            children: &[Element<Str>],
        ) -> Result<Element<Str>> {
            let (index, set_index) = use_state(context, 1);
            Ok(Element::native(
                None,
                Str("test".to_string()),
                (),
                children.to_vec(),
            ))
        }
    }
    #[test]
    fn it_creates_element() {
        let renderer = StringRenderer::new();
        let mut reconciler = Reconciler::new(renderer);
        let mut container = reconciler.create_container(StringNode {
            value: "".to_owned(),
            children: vec![],
        });
        let component = Element::component::<List>(
            None,
            (),
            vec![Element::native(None, Str("hello".to_string()), (), vec![])],
        );
        reconciler
            .update_container(&mut container, &component)
            .unwrap();
        tracing::trace!("UHHH");
        // reconciler
        //     .update_container(&mut container, &component)
        //     .unwrap();
        assert_eq!("testhello", &container.to_string());
    }
}
