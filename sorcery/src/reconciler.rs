use crate::{
    AnyComponent, Component, ComponentElement, ComponentId, ComponentUpdate, Dep, Element, Key,
    NativeElement, RenderPrimitive, StoredProps, StoredState,
};
use bumpalo::Bump;
use crossbeam_channel as channel;
use derivative::Derivative;
use generational_arena::{Arena, Index};
use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    future::Future,
    marker::PhantomData,
    sync::Arc,
};
use tokio::sync::{mpsc, RwLock};
use tracing::{debug, error, trace, warn};
use uuid::Uuid;

#[derive(thiserror::Error, Debug)]
pub enum Error<E>
where
    E: std::error::Error + 'static,
{
    #[error("renderer error")]
    RendererError(E),
    #[error(transparent)]
    Sorcery(#[from] crate::Error),
    #[error("invalid fiber TODO ID")]
    InvalidFiber,
}

pub type Result<T, E> = std::result::Result<T, Error<E>>;

struct Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    nodes: Nodes<P, R>,
    root_node_index: NodeIndex,
    fibers: Fibers<P, R>,
    root_fiber_index: FiberIndex,
}

impl<P, R> Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        nodes: Nodes<P, R>,
        root_node_index: NodeIndex,
        fibers: Fibers<P, R>,
        root_fiber_index: FiberIndex,
    ) -> Self {
        Self {
            nodes,
            root_node_index,
            fibers,
            root_fiber_index,
        }
    }

    fn walk(
        &self,
        f: impl FnMut(&Node<P, R>, &Fiber<P, R>, &NodeIndex, &FiberIndex) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        Node::walk_children(&self.nodes, self.root_node_index, |node, node_index| {
            if let Some(fiber) = self.fibers.get(*node.fiber) {
                f(node, fiber, node_index, &node.fiber)?;
            }
            Ok(())
        })
    }

    fn walk_mut(
        &self,
        f: impl FnMut(
            &mut Node<P, R>,
            &mut Fiber<P, R>,
            &NodeIndex,
            &FiberIndex,
        ) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        Node::walk_children_mut(&mut self.nodes, self.root_node_index, |node, node_index| {
            if let Some(fiber) = self.fibers.get_mut(*node.fiber) {
                f(node, fiber, node_index, &node.fiber)?;
            }
            Ok(())
        })
    }
}

pub struct Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    container: R::Container,
    element_type: std::marker::PhantomData<P>,
    renderer: R,
    events_rx: mpsc::UnboundedReceiver<Event<P, R>>,
    events_tx: mpsc::UnboundedSender<Event<P, R>>,
    component_events_rx: mpsc::UnboundedReceiver<ComponentUpdate>,
    component_events_tx: mpsc::UnboundedSender<ComponentUpdate>,
    // current_tree: bumpalo::collections::Vec<Fiber<P, R>>,
    // new_tree: bumpalo::collections::Vec<Fiber<P, R>>
}

type Fibers<P, R> = Arena<Fiber<P, R>>;
type SharedFibers<P, R> = Arc<RwLock<Fibers<P, R>>>;
type Nodes<P, R> = Arena<Node<P, R>>;
type SharedNodes<P, R> = Arc<RwLock<Nodes<P, R>>>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum Event<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    Render {
        fibers: Arena<Fiber<P, R>>,
        root_id: FiberIndex,
        element: Element<P>,
    },
    ReRender {
        fibers: Arena<Fiber<P, R>>,
    },
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Hash, Eq)]
struct FiberId(Uuid);

impl FiberId {
    fn gen() -> Self {
        Self(Uuid::new_v4())
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
struct Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    id: FiberId,
    body: Option<FiberBody<P, R>>,
    // props: Box<dyn StoredProps>,
    // state: Box<dyn StoredState>,
    // children: Vec<Element<P>>,
    dirty: bool,
}

struct Node<P, R> {
    fiber: FiberIndex,
    // this id points to the other tree
    alternate: Option<NodeIndex>,
    sibling: Option<NodeIndex>,
    child: Option<NodeIndex>,
    parent: Option<NodeIndex>,
    primitive_type: PhantomData<P>,
    renderer_type: PhantomData<R>,
}

#[derive(Debug, Copy, Clone)]
struct FiberIndex(Index);

impl std::ops::Deref for FiberIndex {
    type Target = Index;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct NodeIndex(Index);

impl std::ops::Deref for NodeIndex {
    type Target = Index;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<P, R> Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
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

    fn native_instance_key(&self) -> Option<&R::InstanceKey> {
        match &self.body {
            Some(FiberBody::Native {
                native_instance_key,
                ..
            }) => native_instance_key.as_ref(),
            _ => None,
        }
    }

    fn text_instance_key(&self) -> Option<&R::TextInstanceKey> {
        match &self.body {
            Some(FiberBody::Text(_, instance)) => instance.as_ref(),
            _ => None,
        }
    }

    fn mark_dirty(&mut self) {
        self.dirty = true;
    }
}

// fn process_wrap<P, R>(
//     mut f: impl FnMut(&Fiber<P, R>) -> crate::Result<()>,
// ) -> impl FnMut(&Fiber<P, R>) -> crate::Result<Option<NodeIndex>>
// where
//     P: RenderPrimitive,
//     R: Renderer<P>,
// {
//     move |fiber| {
//         f(fiber)?;
//         Ok(fiber.child)
//     }
// }

impl<P, R> Node<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn walk_children(
        nodes: &Arena<Self>,
        start: NodeIndex,
        process: impl Fn(&Self, &NodeIndex) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        Self::walk(nodes, start, move |node, id| {
            process(node, id)?;
            Ok(node.child)
        })
    }

    fn walk_children_mut(
        nodes: &mut Arena<Self>,
        start: NodeIndex,
        mut process: impl FnMut(&mut Self, &NodeIndex) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        Self::walk_mut(nodes, start, move |node, id| {
            process(node, id)?;
            Ok(node.child)
        })
    }

    fn walk(
        nodes: &Arena<Self>,
        start: NodeIndex,
        mut process: impl FnMut(&Self, &NodeIndex) -> Result<Option<NodeIndex>, R::Error>,
    ) -> Result<(), R::Error> {
        let root = start;
        let mut current = start;
        loop {
            let child = if let Some(node) = nodes.get(*current) {
                process(node, &current)?
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
                let current_node = nodes.get(*current).unwrap(); // todo
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
            let current_node = nodes.get(*current).unwrap(); // todo

            if let Some(sibling) = current_node.sibling {
                current = sibling;
            }
        }
    }

    fn walk_mut(
        nodes: &mut Arena<Self>,
        start: NodeIndex,
        mut process: impl FnMut(&mut Self, &NodeIndex) -> Result<Option<NodeIndex>, R::Error>,
    ) -> Result<(), R::Error> {
        let root = start;
        let mut current = start;
        loop {
            let child = if let Some(current_fiber) = nodes.get_mut(*current) {
                process(current_fiber, &current)?
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
                let current_node = nodes.get(*current).unwrap(); // todo
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
            let current_node = nodes.get(*current).unwrap(); // todo

            if let Some(sibling) = current_node.sibling {
                current = sibling;
            }
        }
    }

    fn parent<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.parent.and_then(|p| arena.get(*p))
    }

    fn sibling<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.sibling.and_then(|p| arena.get(*p))
    }

    fn fiber<'a>(&self, arena: &'a Arena<Fiber<P, R>>) -> Option<&'a Fiber<P, R>> {
        arena.get(*self.fiber)
    }

    fn is_native(&self, arena: &Arena<Fiber<P, R>>) -> bool {
        self.fiber(arena).map_or(false, |f| f.is_native())
    }

    fn is_root(&self, arena: &Arena<Fiber<P, R>>) -> bool {
        self.fiber(arena).map_or(false, |f| f.is_root())
    }

    fn parent_native<'a>(
        &self,
        arena: &'a Arena<Self>,
        fibers: &'a Arena<Fiber<P, R>>,
    ) -> Option<&'a Self> {
        let mut parent = self.parent(arena);
        while let Some(p) = parent {
            if p.is_native(fibers) {
                return Some(p);
            }
            parent = p.parent(arena);
        }
        None
    }

    fn parent_native_mut<'a>(
        &self,
        arena: &'a mut Arena<Self>,
        fibers: &'a Arena<Fiber<P, R>>,
    ) -> Option<&'a mut Self> {
        let index = self.parent_native_index(arena, fibers)?;
        arena.get_mut(*index)
    }

    fn parent_native_index<'a>(
        &'a self,
        nodes: &'a Arena<Self>,
        fibers: &'a Arena<Fiber<P, R>>,
    ) -> Option<NodeIndex> {
        let mut parent = self.parent;
        while let Some(p) = parent {
            if let Some(node) = nodes.get(*p) {
                if node.is_native(fibers) {
                    return Some(p);
                }
                parent = node.parent;
            }
        }
        None
    }

    fn children(&self, nodes: &Arena<Self>) -> Vec<NodeIndex> {
        let mut indexes = vec![];
        if let Some(first) = self.child {
            let mut current = Some(first);
            loop {
                if let Some(node) = current.and_then(|index| nodes.get(*index)) {
                    indexes.push(current.unwrap());
                    current = node.sibling;
                } else {
                    break;
                }
            }
        }
        indexes
    }
}

impl<P, R> PartialEq<Fiber<P, R>> for Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn eq(&self, other: &Fiber<P, R>) -> bool {
        self.id == other.id
    }
}

#[derive(Debug, Clone)]
struct FiberComponentContext {
    id: FiberId,
}

impl FiberComponentContext {
    fn new(id: FiberId) -> Self {
        Self { id }
    }
}

impl<'r, P, R> Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(body: FiberBody<P, R>) -> Self {
        Self {
            id: FiberId::gen(),
            dirty: false,
            body: Some(body),
        }
    }

    fn text(text: String) -> Self {
        Self::new(FiberBody::Text(text, None))
    }

    fn component(
        instance: Box<dyn AnyComponent<P>>,
        props: Box<dyn StoredProps>,
        children: Vec<Element<P>>,
    ) -> Self {
        let body = FiberBody::Component {
            instance,
            props,
            children,
        };
        Self::new(body)
    }

    fn native(instance: P, props: P::Props, children: Vec<Element<P>>) -> Self {
        let body = FiberBody::Native {
            instance,
            props,
            native_instance_key: None,
            children,
        };
        Self::new(body)
    }

    fn children(&self) -> Vec<Element<P>> {
        match &self.body {
            Some(FiberBody::Component { children, .. }) => children.clone(),
            Some(FiberBody::Native { children, .. }) => children.clone(),
            _ => vec![],
        }
    }

    fn render(&mut self) -> crate::Result<Vec<Element<P>>> {
        let children = match &self.body {
            Some(FiberBody::Root) => {
                unimplemented!("root");
            }
            Some(FiberBody::Text(_, _)) => Ok(vec![]),
            Some(FiberBody::Component {
                instance, props, ..
            }) => {
                let children = self.children();
                Ok(vec![instance.render(props.as_ref(), &children)?])
            }
            Some(FiberBody::Native {
                instance, props, ..
            }) => Ok(instance.render(&props, &self.children())?),
            None => {
                unimplemented!();
            }
        }?;

        Ok(children)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
enum FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    Root,
    Text(String, Option<R::TextInstanceKey>),
    Component {
        instance: Box<dyn AnyComponent<P>>,
        props: Box<dyn StoredProps>,
        children: Vec<Element<P>>,
    },
    Native {
        instance: P,
        native_instance_key: Option<R::InstanceKey>,
        props: P::Props,
        children: Vec<Element<P>>,
    },
}

impl<P, R> PartialEq<FiberBody<P, R>> for FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn eq(&self, other: &FiberBody<P, R>) -> bool {
        warn!("IMPLEMENT PARTIALEQ FOR REAL");
        match (self, other) {
            (FiberBody::Root, FiberBody::Root) => true,
            (FiberBody::Text(t, _), FiberBody::Text(t2, _)) if t == t2 => true,
            _ => false,
        }
    }
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    pub fn new(renderer: R, container: R::Container) -> Self {
        let (events_tx, events_rx) = mpsc::unbounded_channel();
        let (component_events_tx, component_events_rx) = mpsc::unbounded_channel();
        Self {
            container,
            renderer,
            element_type: std::marker::PhantomData,
            events_rx,
            events_tx,
            component_events_rx,
            component_events_tx,
        }
    }

    pub fn create_container(&mut self, base: &mut R::Container) {}

    fn create_instances(&mut self, tree: &Tree<P, R>) -> Result<(), R::Error> {
        tree.walk_mut(|_, fiber, _, _| {
            match &fiber.body {
                Some(FiberBody::Text(txt, ref mut instance_key)) if instance_key.is_none() => {
                    *instance_key = Some(
                        self.renderer
                            .create_text_instance(&txt)
                            .map_err(Error::RendererError)?,
                    );
                }
                Some(FiberBody::Native {
                    ref mut native_instance_key,
                    instance,
                    props,
                    ..
                }) if native_instance_key.is_none() => {
                    *native_instance_key = Some(
                        self.renderer
                            .create_instance(instance, props)
                            .map_err(Error::RendererError)?,
                    );
                }
                _ => {}
            };
            Ok(())
        })?;

        walk_fibers_mut(
            fibers,
            root_id,
            process_wrap_mut(|fiber, _| {
                match &mut fiber.body {
                    Some(FiberBody::Text(txt, ref mut instance_key)) if instance_key.is_none() => {
                        *instance_key = Some(
                            self.renderer
                                .borrow_mut()
                                .create_text_instance(&txt)
                                .map_err(Error::RendererError)?,
                        );
                    }
                    Some(FiberBody::Native {
                        ref mut native_instance_key,
                        instance,
                        props,
                        ..
                    }) if native_instance_key.is_none() => {
                        *native_instance_key = Some(
                            self.renderer
                                .borrow_mut()
                                .create_instance(instance, props)
                                .map_err(Error::RendererError)?,
                        );
                    }
                    _ => {}
                };
                Ok(())
            }),
        )?;
        Ok(())
    }

    fn diff(&self, new_fibers: &Arena<Fiber<P, R>>, new_root_id: ArenaNodeId) -> Vec<DiffOp> {
        fn diff_fiber<P, R>(
            current_fibers: &Arena<Fiber<P, R>>,
            (current, current_id): (&Fiber<P, R>, &ArenaNodeId),
            new_fibers: &Arena<Fiber<P, R>>,
            (new, new_id): (&Fiber<P, R>, &ArenaNodeId),
            ops: &mut Vec<DiffOp>,
        ) where
            P: RenderPrimitive,
            R: Renderer<P>,
        {
            debug!("comparing {:?} and {:?}", current, new);
            if current != new {
                debug!("neq, replace");
                ops.push(DiffOp::Remove { id: *current_id });
                ops.push(DiffOp::Append { id: *new_id });
            }
            let current_children = current.child_fibers(current_fibers);
            let new_children = new.child_fibers(new_fibers);
            if current_children.len() == new_children.len() {
                for index in 0..new_children.len() {
                    let current_index = current_children[index];
                    let current_root = current_fibers.get(current_index).unwrap();
                    let new_index = new_children[index];
                    let new_root = new_fibers.get(new_index).unwrap();
                    diff_fiber(
                        current_fibers,
                        (current_root, &current_index),
                        new_fibers,
                        (new_root, &new_index),
                        ops,
                    );
                }
            } else {
                unimplemented!()
            }
        };
        let mut ops = Vec::new();
        if let Some((current_fibers, current_root_id)) = &self.current_tree {
            let current_root = current_fibers.get(*current_root_id).unwrap();
            let new_root = new_fibers.get(new_root_id).unwrap();
            diff_fiber(
                current_fibers,
                (current_root, current_root_id),
                new_fibers,
                (new_root, &new_root_id),
                &mut ops,
            );
        } else {
            // add everything
        }
        ops
    }

    fn commit(
        &mut self,
        mut fibers: Arena<Fiber<P, R>>,
        root_id: ArenaNodeId,
    ) -> Result<(), R::Error> {
        let (tx, rx) = channel::unbounded::<Update>();

        self.create_instances(&mut fibers, root_id)?;
        let diff = self.diff(&fibers, root_id);

        debug!("DIFF {:?}", diff);

        if self.current_tree.is_none() {
            walk_fibers(&fibers, root_id, |fiber, id| {
                match &fiber.body {
                    Some(FiberBody::Text(text, Some(text_instance))) => {
                        let parent = fiber.parent_native(&fibers);
                        if parent.as_ref().map_or(false, |p| p.is_native()) {
                            if parent.as_ref().map_or(false, |p| p.is_root()) {
                                debug!("append text to container?");
                            } else if let Some(_) = parent.and_then(|p| p.native_instance_key()) {
                                debug!("append text ({:?}) to parent", text);
                                tx.send(Update::AppendTextToParent {
                                    parent: fiber.parent_native_id(&fibers).unwrap(),
                                    text: *id,
                                })
                                .unwrap();
                                // self.renderer
                                //     .append_child_to_parent(parent, native_instance);
                            }
                        }
                    }
                    Some(FiberBody::Native {
                        native_instance_key: Some(native_instance_key),
                        ..
                    }) => {
                        let parent = fiber.parent_native(&fibers);
                        if parent.as_ref().map_or(false, |p| p.is_native()) {
                            if parent.as_ref().map_or(false, |p| p.is_root()) {
                                debug!("append to container");
                                tx.send(Update::AppendChildToContainer { child: *id })
                                    .unwrap();
                            } else if let Some(_) = parent.and_then(|p| p.native_instance_key()) {
                                debug!("append to child");
                                tx.send(Update::AppendChildToParent {
                                    parent: fiber.parent_native_id(&fibers).unwrap(),
                                    child: *id,
                                })
                                .unwrap();
                                // self.renderer
                                //     .append_child_to_parent(parent, native_instance);
                            }
                        }
                    }
                    _ => {}
                }
                Ok(fiber.child)
            })?;
        } else {
            for op in diff {
                // debug!("diff op: {:?}", op);
                match op {
                    DiffOp::Remove { id } => {
                        if let Some(fiber) = fibers.get(id) {
                            // debug!("remove fiber: {:?}", fiber);
                            if let Some(parent) = fiber.parent_native(&fibers) {
                                if parent.is_root() {
                                    match &fiber.body {
                                        Some(FiberBody::Text(text, Some(text_instance))) => {}
                                        Some(FiberBody::Native {
                                            native_instance_key,
                                            ..
                                        }) => {
                                            tx.send(Update::RemoveChildFromContainer { child: id })
                                                .unwrap();
                                        }
                                        other => {
                                            warn!("todo: rm root for {:?}", other);
                                        }
                                    };
                                } else {
                                    match &fiber.body {
                                        Some(FiberBody::Text(text, Some(text_instance))) => {}
                                        Some(FiberBody::Native {
                                            native_instance_key,
                                            ..
                                        }) => {
                                            tx.send(Update::RemoveChildFromParent {
                                                parent: fiber.parent_native_id(&fibers).unwrap(),
                                                child: id,
                                            })
                                            .unwrap();
                                        }
                                        other => {
                                            warn!("todo: rm non-root for {:?}", other);
                                        }
                                    };
                                }
                            }
                        }
                    }
                    DiffOp::Append { id } => {
                        if let Some(fiber) = fibers.get(id) {
                            // debug!("append fiber: {:?}", fiber);
                            if let Some(parent) = fiber.parent_native(&fibers) {
                                if parent.is_root() {
                                    match &fiber.body {
                                        Some(FiberBody::Text(text, Some(text_instance))) => {
                                            unimplemented!();
                                            // tx.send(Update::AppendTextToContainer { text: id })
                                            //     .unwrap();
                                        }
                                        Some(FiberBody::Native {
                                            native_instance_key,
                                            ..
                                        }) => {
                                            tx.send(Update::AppendChildToContainer { child: id })
                                                .unwrap();
                                        }
                                        other => {
                                            warn!("todo: append root for {:?}", other);
                                        }
                                    };
                                } else {
                                    match &fiber.body {
                                        Some(FiberBody::Text(text, Some(text_instance))) => {
                                            tx.send(Update::AppendTextToParent {
                                                parent: fiber.parent_native_id(&fibers).unwrap(),
                                                text: id,
                                            })
                                            .unwrap();
                                        }
                                        Some(FiberBody::Native {
                                            native_instance_key,
                                            ..
                                        }) => {
                                            tx.send(Update::AppendChildToParent {
                                                parent: fiber.parent_native_id(&fibers).unwrap(),
                                                child: id,
                                            })
                                            .unwrap();
                                        }
                                        other => {
                                            warn!("todo: apppend for non-root {:?}", other);
                                        }
                                    };
                                }
                            }
                        }
                    }
                }
            }
        }

        self.current_tree = Some((fibers, root_id));
        self.process_updates(rx)?;
        Ok(())
    }

    fn process_updates(&mut self, rx: channel::Receiver<Update>) -> Result<(), R::Error> {
        let fibers = &mut self.current_tree.as_mut().unwrap().0;
        for update in rx.try_iter() {
            // trace!("update: {:?}", update);
            match update {
                Update::AppendTextToParent { parent, text } => {
                    let parent = fibers.get(parent).unwrap();

                    let text = fibers.get(text).unwrap();
                    self.renderer
                        .borrow_mut()
                        .append_text_to_parent(
                            parent.native_instance_key().unwrap(),
                            text.text_instance_key().unwrap(),
                        )
                        .map_err(Error::RendererError)?;
                }
                Update::AppendChildToContainer { child } => {
                    self.renderer
                        .borrow_mut()
                        .append_child_to_container(
                            &mut self.container,
                            fibers
                                .get_mut(child)
                                .unwrap()
                                .native_instance_key()
                                .unwrap(),
                        )
                        .map_err(Error::RendererError)?;
                }
                Update::AppendChildToParent { parent, child } => {
                    let (parent, child) = fibers.get2_mut(parent, child);
                    match (parent, child) {
                        (Some(parent), Some(child)) => {
                            self.renderer
                                .borrow_mut()
                                .append_child_to_parent(
                                    parent.native_instance_key().unwrap(),
                                    child.native_instance_key().unwrap(),
                                )
                                .map_err(Error::RendererError)?;
                        }
                        _ => {
                            warn!("todo");
                        }
                    }
                }
                Update::RemoveChildFromContainer { child } => {
                    self.renderer
                        .borrow_mut()
                        .remove_child_from_container(
                            &mut self.container,
                            fibers
                                .get_mut(child)
                                .unwrap()
                                .native_instance_key()
                                .unwrap(),
                        )
                        .map_err(Error::RendererError)?;
                }
                Update::RemoveChildFromParent { parent, child } => {
                    let (parent, child) = fibers.get2_mut(parent, child);
                    debug!("parent? {:?}", parent);
                    debug!("child? {:?}", child);
                    match (parent, child) {
                        (Some(parent), Some(child)) => {
                            self.renderer
                                .borrow_mut()
                                .remove_child_from_parent(
                                    parent.native_instance_key().unwrap(),
                                    child.native_instance_key().unwrap(),
                                )
                                .map_err(Error::RendererError)?;
                        }
                        _ => {
                            warn!("todo");
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub async fn run(&mut self) {
        debug!("running");
        let mut current_element: Option<Element<P>> = None;
        let mut current_root_id: Option<ArenaNodeId> = None;
        loop {
            tokio::select! {
                Some(event) = self.events_rx.recv() => {
                    // debug!("event: {:?}", event);
                match event {
                    Event::Render {
                        mut fibers,
                        root_id,
                        element,
                    } => {
                        if self.current_tree.is_none() {
                            self.commit(fibers, root_id).expect("todo");
                            current_element = Some(element);
                            current_root_id = Some(root_id);
                        } else {
                            debug!("update");
                        }
                    },
                    Event::ReRender {mut fibers} => {
                        self.commit(fibers, current_root_id.clone().unwrap()).expect("todo");
                    }
                }
                }
                Some(event) = self.component_events_rx.recv() => {
                    unimplemented!();
                }
            };
        }
    }

    pub fn update_container(
        &mut self,
        ctx: &mut Context<P, R>,
        element: &Element<P>,
    ) -> Result<(), R::Error> {
        self.renderer.borrow().schedule_task(
            TaskPriority::Immediate,
            Box::new(RenderTask::<P, R>::new(
                self.events_tx.clone(),
                self.component_events_tx.clone(),
                element.clone(),
            )),
        );

        // self.renderer.borrow().schedule_task(Box::new({
        //     let r = self.renderer.clone();
        //     let rx = ctx.rx.clone();
        //     ProcessMessages {
        //         rx: rx.clone(),
        //         renderer: r,
        //         component_type: PhantomData
        //         // done: Box::new({
        //         //     let r = r.clone();
        //         //     let rx = rx.clone();
        //         //     move || {
        //         //         debug!("calling done");
        //         //         r.borrow().schedule_task(Box::new(ProcessMessages {
        //         //             rx: rx.clone(),
        //         //             done: Box::new(|| {}),
        //         //         }));
        //         //     }
        //         // }),
        //     }
        // }));

        // self.renderer.append_child_to_container(container, tree);
        Ok(())
    }
}

fn build_tree<'a, P, R>(
    tx: &mpsc::UnboundedSender<ComponentUpdate>,
    arena: &mut Arena<Fiber<P, R>>,
    element: &Element<P>,
) -> Result<ArenaNodeId, R::Error>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    // debug!("rendering a {:?}\n\n", element);
    let children = element.children();
    let fiber = match element {
        Element::Text(txt) => {
            let fiber = Fiber::text(tx.clone(), FiberId::gen(), txt.to_owned());
            Ok(fiber)
        }
        Element::Component(comp_element) => {
            let instance = comp_element.construct().map_err(Error::Sorcery)?;
            let fiber = Fiber::component(
                tx.clone(),
                FiberId::gen(),
                instance,
                comp_element.clone_props(),
                children.to_vec(),
            );
            Result::<_, R::Error>::Ok(fiber)
        }
        Element::Native(native) => {
            let instance = native.ty.clone();
            let fiber = Fiber::native(
                tx.clone(),
                FiberId::gen(),
                instance,
                native.props.clone(),
                children.to_vec(),
            );
            Ok(fiber)
        }
    }?;
    let id = arena.insert(fiber);
    {
        let to_child = {
            let node = arena.get_mut(id).unwrap();
            node.render()
                .map_err(Error::Sorcery)?
                .into_iter()
                .rev()
                .fold(Result::<_, R::Error>::Ok(None), |prev, next| {
                    let child_id = build_tree(tx, arena, &next)?;
                    let mut child = arena.get_mut(child_id).unwrap();
                    child.parent = Some(id.clone());
                    child.sibling = prev?;
                    Ok(Some(child_id))
                })?
        };
        let mut node = arena.get_mut(id).unwrap();
        node.child = to_child;
    }
    Ok(id)
}

type TaskResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;
struct RenderTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    root: Element<P>,
    tx: mpsc::UnboundedSender<Event<P, R>>,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
}

impl<P, R> RenderTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        tx: mpsc::UnboundedSender<Event<P, R>>,
        component_tx: mpsc::UnboundedSender<ComponentUpdate>,
        root: Element<P>,
    ) -> Self {
        Self {
            root,
            tx,
            component_tx,
        }
    }
}

impl<P, R> Task for RenderTask<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    fn run(self: Box<Self>) -> TaskResult<()> {
        // debug!("rendering {:?}", self.root);
        let mut fibers = Arena::<Fiber<P, R>>::new();
        let root_id = {
            let fiber_id = FiberId::gen();
            let node_id = build_tree(&self.component_tx, &mut fibers, &self.root)?;
            let mut root_fiber = Fiber {
                dirty: false,
                body: Some(FiberBody::Root),
                id: fiber_id,
                alternate: None,
                child: None,
                parent: None,
                sibling: None,
            };
            root_fiber.child = Some(node_id);
            let root_id = fibers.insert(root_fiber);
            fibers.get_mut(node_id).unwrap().parent = Some(root_id);
            root_id
        };
        self.tx
            .send(Event::Render {
                fibers,
                root_id,
                element: self.root,
            })
            .unwrap();
        Ok(())
    }
}

struct Render2Task<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    tx: mpsc::UnboundedSender<Event<P, R>>,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
    fibers: Arena<Fiber<P, R>>,
    dirty_fiber_id: ArenaNodeId,
}

impl<P, R> Render2Task<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        tx: mpsc::UnboundedSender<Event<P, R>>,
        component_tx: mpsc::UnboundedSender<ComponentUpdate>,
        fibers: Arena<Fiber<P, R>>,
        dirty_fiber_id: ArenaNodeId,
    ) -> Self {
        Self {
            tx,
            component_tx,
            dirty_fiber_id,
            fibers,
        }
    }
}

impl<P, R> Task for Render2Task<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    fn run(mut self: Box<Self>) -> TaskResult<()> {
        debug!("rerendering {:?}", self.dirty_fiber_id);
        // walk_fibers_mut(
        //     &mut fibers,
        //     self.root_fiber_id,
        //     process_wrap_mut(|fiber| {
        //         if fiber.dirty {
        //             debug!("fiber is dirty {:?}", fiber);

        //         }
        //         Result::<_, R::Error>::Ok(())
        //     }),
        // )?;

        let new_children = self
            .fibers
            .get_mut(self.dirty_fiber_id)
            .ok_or(Error::<R::Error>::InvalidFiber)
            .and_then(|f| {
                debug!("rerendering {:?}", f);

                Ok(f.render()?)
            })?;

        debug!("new children {:?}", new_children);
        let alternate: Fiber<P, R> = self.fibers.get(self.dirty_fiber_id).unwrap().clone();
        let alternate_id = self.fibers.insert(alternate);
        let mut fibers = &mut self.fibers;
        let tx = self.component_tx.clone();
        let dirty_fiber_id = self.dirty_fiber_id.clone();
        let to_child = {
            new_children.into_iter().rev().fold(
                Result::<_, R::Error>::Ok(None),
                move |prev, next| {
                    let child_id = build_tree(&tx, fibers, &next)?;
                    let mut child = &mut fibers.get_mut(child_id).unwrap();
                    child.parent = Some(alternate_id);
                    child.sibling = prev?;
                    Ok(Some(child_id))
                },
            )?
        };
        let mut node = self.fibers.get_mut(self.dirty_fiber_id).unwrap();
        node.alternate = Some(alternate_id);
        self.tx
            .send(Event::ReRender {
                fibers: self.fibers,
            })
            .unwrap();

        Ok(())
    }
}

struct Render3Task<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    tx: mpsc::UnboundedSender<Event<P, R>>,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
    fibers: Arena<Fiber<P, R>>,
    root_fiber_id: ArenaNodeId,
}

impl<P, R> Render3Task<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        tx: mpsc::UnboundedSender<Event<P, R>>,
        component_tx: mpsc::UnboundedSender<ComponentUpdate>,
        fibers: Arena<Fiber<P, R>>,
        root_fiber_id: ArenaNodeId,
    ) -> Self {
        Self {
            tx,
            component_tx,
            root_fiber_id,
            fibers,
        }
    }
}

impl<P, R> Task for Render3Task<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    fn run(mut self: Box<Self>) -> TaskResult<()> {
        debug!("rerendering {:?}", self.root_fiber_id);
        // walk_fibers_mut(
        //     &mut fibers,
        //     self.root_fiber_id,
        //     process_wrap_mut(|fiber| {
        //         if fiber.dirty {
        //             debug!("fiber is dirty {:?}", fiber);

        //         }
        //         Result::<_, R::Error>::Ok(())
        //     }),
        // )?;

        let component_tx = self.component_tx.clone();

        let (tx, rx) = channel::unbounded::<RenderUpdate<P>>();

        walk_fibers_mut(
            &mut self.fibers,
            self.root_fiber_id,
            process_wrap_mut(move |fiber, fiber_id| {
                if fiber.dirty {
                    let new_children = fiber.render()?;
                    tx.send(RenderUpdate::Update {
                        parent: *fiber_id,
                        new_children,
                    })
                    .unwrap();
                    fiber.dirty = false;
                }
                Result::<_, R::Error>::Ok(())
            }),
        )?;

        for update in rx.try_iter() {
            match update {
                RenderUpdate::Update {
                    parent,
                    new_children,
                } => {
                    debug!("updating {:?}", parent);
                    let tx = self.component_tx.clone();
                    let to_child = {
                        let fibers = &mut self.fibers;
                        new_children.into_iter().rev().fold(
                            Result::<_, R::Error>::Ok(None),
                            move |prev, next| {
                                let child_id = build_tree(&tx, fibers, &next)?;
                                let mut child = fibers.get_mut(child_id).unwrap();
                                child.parent = Some(parent);
                                child.sibling = prev?;
                                Ok(Some(child_id))
                            },
                        )?
                    };
                    let fiber = self.fibers.get_mut(parent).unwrap();
                    fiber.child = to_child;
                }
            }
        }

        self.tx
            .send(Event::ReRender {
                fibers: self.fibers,
            })
            .unwrap();

        Ok(())
    }
}

enum RenderUpdate<P>
where
    P: RenderPrimitive,
{
    Update {
        parent: ArenaNodeId,
        new_children: Vec<Element<P>>,
    },
}

// struct ProcessMessages<P, R>
// where
//     R: Renderer<P>,
//     P: RenderPrimitive,
// {
//     rx: channel::Receiver<ComponentUpdate>,
//     renderer: Arc<RefCell<R>>,
//     component_type: PhantomData<P>,
// }

// impl<P, R> Task for ProcessMessages<P, R>
// where
//     P: RenderPrimitive + 'static,
//     R: Renderer<P> + 'static,
// {
//     fn run(self: Box<Self>) {
//         for msg in self.rx.try_iter() {
//             debug!("update: {:?}", msg);
//         }
//         self.renderer
//             .clone()
//             .borrow()
//             .schedule_task(Box::new(ProcessMessages {
//                 rx: self.rx,
//                 renderer: self.renderer,
//                 component_type: self.component_type,
//             }));
//     }
// }

pub struct Context<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fibers: Arena<Fiber<P, R>>,
    tx: channel::Sender<ComponentUpdate>,
    rx: channel::Receiver<ComponentUpdate>,
}

impl<'r, P, R> Context<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    pub fn new() -> Self {
        let (tx, rx) = channel::unbounded();
        Self {
            fibers: Arena::new(),
            tx,
            rx,
        }
    }
}

#[derive(Debug)]
enum Update {
    AppendChildToContainer {
        child: ArenaNodeId,
    },
    AppendChildToParent {
        parent: ArenaNodeId,
        child: ArenaNodeId,
    },

    RemoveChildFromParent {
        parent: ArenaNodeId,
        child: ArenaNodeId,
    },
    RemoveChildFromContainer {
        child: ArenaNodeId,
    },
    AppendTextToParent {
        parent: ArenaNodeId,
        text: ArenaNodeId,
    },
}

#[derive(Debug)]
pub enum Op {}

pub trait RendererContext {
    fn context_for() -> Self;
}

// TODO <P> should probably be an associated type (Primitive: RenderPrimitive)

pub trait Renderer<P>
where
    P: RenderPrimitive,
{
    type Error: std::error::Error + 'static;
    type Container;
    type InstanceKey: Clone + std::fmt::Debug;
    type TextInstanceKey: Clone + std::fmt::Debug;
    fn create_instance(
        &mut self,
        ty: &P,
        props: &P::Props,
    ) -> std::result::Result<Self::InstanceKey, Self::Error>;
    fn create_text_instance(
        &mut self,
        text: &str,
    ) -> std::result::Result<Self::TextInstanceKey, Self::Error>;
    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn append_text_to_parent(
        &mut self,
        parent: &Self::InstanceKey,
        text: &Self::TextInstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn append_child_to_parent<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn remove_child_from_parent<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn remove_child_from_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn schedule_task(&self, priority: TaskPriority, task: Box<dyn Task>);
}

#[derive(Debug, Copy, Clone)]
pub enum TaskPriority {
    Immediate,
    Idle,
}

pub trait Task {
    fn run(self: Box<Self>) -> TaskResult<()>;
}
