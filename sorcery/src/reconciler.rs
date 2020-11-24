use crate::{
    AnyComponent, Component, ComponentElement, ComponentId, ComponentUpdate, Dep, Element, Key,
    NativeElement, RenderPrimitive, StoredProps, StoredState,
};
use async_trait::async_trait;
use bumpalo::Bump;
use crossbeam_channel as channel;
use derivative::Derivative;
use generational_arena::{Arena, Index};
use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    fmt,
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

impl<P, R> fmt::Debug for Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();
        let r = &mut list;
        self.walk(move |node, fiber, node_index, fiber_index| {
            r.entry(fiber);
            Ok(())
        });
        list.finish()?;
        Ok(())
    }
}

impl<P, R> Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn empty() -> Self {
        let mut nodes = Arena::new();
        let mut fibers = Arena::new();
        let (root_node_index, root_fiber_index) = create_root(&mut nodes, &mut fibers);
        Self::new(nodes, root_node_index, fibers, root_fiber_index)
    }

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
        mut f: impl FnMut(&Node<P, R>, &Fiber<P, R>, &NodeIndex, &FiberIndex) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        Node::walk_children(&self.nodes, self.root_node_index, |node, node_index| {
            if let Some(fiber) = self.fibers.get(*node.fiber) {
                f(node, fiber, node_index, &node.fiber)?;
            }
            Ok(())
        })
    }

    fn walk_mut(
        &mut self,
        mut f: impl FnMut(
            &mut Node<P, R>,
            &mut Fiber<P, R>,
            &NodeIndex,
            &FiberIndex,
        ) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        let fibers = &mut self.fibers;
        let nodes = &mut self.nodes;
        Node::walk_children_mut(nodes, self.root_node_index, move |node, node_index| {
            let fiber_index = node.fiber;
            if let Some(fiber) = fibers.get_mut(*node.fiber) {
                f(node, fiber, node_index, &fiber_index)?;
            }
            Ok(())
        })
    }

    fn walk_with_mut_fibers(
        &mut self,
        mut f: impl FnMut(
            &Node<P, R>,
            &mut Fiber<P, R>,
            &NodeIndex,
            &FiberIndex,
            &Arena<Node<P, R>>,
        ) -> Result<(), R::Error>,
    ) -> Result<(), R::Error> {
        let fibers = &mut self.fibers;
        let nodes = &self.nodes;
        Node::walk_children(&nodes, self.root_node_index, move |node, node_index| {
            if let Some(fiber) = fibers.get_mut(*node.fiber) {
                f(node, fiber, node_index, &node.fiber, &nodes)?;
            }
            Ok(())
        })
    }

    fn root_fiber(&self) -> &Fiber<P, R> {
        self.fibers.get(*self.root_fiber_index).unwrap()
    }

    fn child_fibers(&self, fiber: &Fiber<P, R>) -> Vec<&Fiber<P, R>> {
        if let Some(node_index) = fiber.node_index {
            self.nodes
                .get(*node_index)
                .map(|node| node.children(&self.nodes))
                .unwrap_or(vec![])
                .into_iter()
                .filter_map(|node_index| self.nodes.get(*node_index).map(|node| node.fiber))
                .filter_map(|fiber_index| self.fibers.get(*fiber_index))
                .collect()
        } else {
            vec![]
        }
    }

    fn native_parent(&self, fiber: &Fiber<P, R>) -> Option<&Fiber<P, R>> {
        fiber.native_parent(&self.nodes, &self.fibers)
    }

    fn walk_diff<RF, AF>(
        &self,
        other: &Self,
        walker: &mut DiffWalker<P, R, RF, AF>,
    ) -> Result<(), R::Error>
    where
        RF: FnMut(&Fiber<P, R>, &Fiber<P, R>) -> Result<(), R::Error>,
        AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    {
        fn walk_fiber<P, R, RF, AF>(
            left: &Tree<P, R>,
            left_fiber: Option<&Fiber<P, R>>,
            right: &Tree<P, R>,
            right_fiber: Option<&Fiber<P, R>>,
            walker: &mut DiffWalker<P, R, RF, AF>,
        ) -> Result<(), R::Error>
        where
            P: RenderPrimitive,
            R: Renderer<P>,
            RF: FnMut(&Fiber<P, R>, &Fiber<P, R>) -> Result<(), R::Error>,
            AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
        {
            match (left_fiber, right_fiber) {
                (Some(left_fiber), Some(right_fiber)) => {
                    debug!("comparing {:?} vs {:?}", left_fiber, right_fiber);
                    if left_fiber != right_fiber {
                        (walker.replace)(left_fiber, right_fiber)?;
                    }
                    let left_children = left.child_fibers(left_fiber);
                    let right_children = right.child_fibers(right_fiber);
                    debug!("{:?} -> {:?}", left_children, right_children);
                    match (left_children, right_children) {
                        (left_children, right_children)
                            if left_children.len() == right_children.len() => {}
                        (left_children, right_children) if left_children.len() == 0 => {
                            for child in right_children {
                                walk_fiber(left, None, right, Some(child), walker)?;
                            }
                        }
                        (left_children, right_children) if right_children.len() == 0 => {
                            unimplemented!("all rm children");
                        }
                        (left_children, right_children) => {
                            unimplemented!("uneven children");
                        }
                    }
                }
                (None, Some(right_fiber)) => {
                    (walker.append)(right_fiber, right)?;
                    for child in right.child_fibers(right_fiber) {
                        walk_fiber(left, None, right, Some(child), walker)?;
                    }
                }
                (Some(left_fiber), None) => {
                    unimplemented!("no right");
                }
                (None, None) => {
                    // nothing to do
                }
            }
            Ok(())
        }
        let root_fiber = self.root_fiber();
        let other_root_fiber = other.root_fiber();
        walk_fiber(
            self,
            Some(root_fiber),
            other,
            Some(other_root_fiber),
            walker,
        )?;
        Ok(())
    }
}

struct DiffWalker<P, R, RF, AF>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    RF: FnMut(&Fiber<P, R>, &Fiber<P, R>) -> Result<(), R::Error>,
    AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
{
    replace: RF,
    append: AF,
    p: PhantomData<P>,
    r: PhantomData<R>,
}

pub struct Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    container: R::Container,
    renderer: Arc<RefCell<R>>,
    events_rx: mpsc::UnboundedReceiver<Event<P, R>>,
    events_tx: mpsc::UnboundedSender<Event<P, R>>,
    component_events_rx: mpsc::UnboundedReceiver<ComponentUpdate>,
    component_events_tx: mpsc::UnboundedSender<ComponentUpdate>,
    current_tree: Option<Tree<P, R>>,
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
        tree: Tree<P, R>,
        element: Element<P>,
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
    node_index: Option<NodeIndex>,
    body: Option<FiberBody<P, R>>,
    // props: Box<dyn StoredProps>,
    // state: Box<dyn StoredState>,
    // children: Vec<Element<P>>,
    dirty: bool,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
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

impl<P, R> Node<P, R> {
    fn new(fiber: FiberIndex) -> Self {
        Self {
            fiber,
            alternate: None,
            sibling: None,
            child: None,
            parent: None,
            primitive_type: PhantomData,
            renderer_type: PhantomData,
        }
    }
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

    fn node<'n>(&self, nodes: &'n Arena<Node<P, R>>) -> Option<&'n Node<P, R>> {
        self.node_index.and_then(|index| nodes.get(*index))
    }

    fn native_parent<'n>(
        &self,
        nodes: &'n Arena<Node<P, R>>,
        fibers: &'n Arena<Fiber<P, R>>,
    ) -> Option<&'n Fiber<P, R>> {
        self.node(nodes)
            .and_then(|node| node.native_parent(nodes, fibers))
            .and_then(|node| node.fiber(fibers))
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
        mut process: impl FnMut(&Self, &NodeIndex) -> Result<(), R::Error>,
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

    fn native_parent<'a>(
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

    fn native_parent_mut<'a>(
        &self,
        arena: &'a mut Arena<Self>,
        fibers: &'a Arena<Fiber<P, R>>,
    ) -> Option<&'a mut Self> {
        let index = self.native_parent_index(arena, fibers)?;
        arena.get_mut(*index)
    }

    fn native_parent_index<'a>(
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
        self.body == other.body
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
            node_index: None,
            dirty: false,
            body: Some(body),
        }
    }

    fn root() -> Self {
        Self::new(FiberBody::Root)
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

    fn children(&self) -> Option<&[Element<P>]> {
        match &self.body {
            Some(FiberBody::Component { children, .. }) => Some(&children),
            Some(FiberBody::Native { children, .. }) => Some(&children),
            _ => None,
        }
    }

    fn clone_children(&self) -> Vec<Element<P>> {
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
                let children = self.children().unwrap_or(&[]);
                Ok(vec![instance.render(props.as_ref(), children)?])
            }
            Some(FiberBody::Native {
                instance, props, ..
            }) => Ok(instance.render(&props, self.children().unwrap_or(&[]))?),
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
            (FiberBody::Root, FiberBody::Root) => {
                // roots are always equal
                true
            }
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
            renderer: Arc::new(RefCell::new(renderer)),
            events_rx,
            events_tx,
            component_events_rx,
            component_events_tx,
            current_tree: None,
        }
    }

    fn create_instances(&mut self, tree: &mut Tree<P, R>) -> Result<(), R::Error> {
        debug!("creating instances");
        tree.walk_with_mut_fibers(|_, fiber, _, _, _| {
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
        })?;
        Ok(())
    }

    fn commit<'r>(&'r mut self, mut tree: Tree<P, R>) -> Result<(), R::Error> {
        debug!("committing");
        self.create_instances(&mut tree)?;
        let mut container = &mut self.container;
        let renderer = self.renderer.clone();
        let mut walker = DiffWalker {
            p: PhantomData,
            r: PhantomData,
            replace: |old, new| Ok(()),
            append: move |child, tree| {
                if let Some(parent) = tree.native_parent(child) {
                    debug!("append {:?} to {:?}", child, parent);
                    match &child.body {
                        Some(FiberBody::Text(text, Some(text_instance_key))) => {
                            if parent.is_root() {
                                unimplemented!();
                            } else {
                                renderer
                                    .borrow_mut()
                                    .append_text_to_parent(
                                        parent.native_instance_key().unwrap(),
                                        text_instance_key,
                                    )
                                    .map_err(Error::RendererError)?;
                            }
                        }
                        Some(FiberBody::Native {
                            native_instance_key: Some(native_instance_key),
                            ..
                        }) => {
                            if parent.is_root() {
                                renderer
                                    .borrow_mut()
                                    .append_child_to_container(&mut container, native_instance_key)
                                    .map_err(Error::RendererError)?;
                            } else {
                                renderer
                                    .borrow_mut()
                                    .append_child_to_parent(
                                        parent.native_instance_key().unwrap(),
                                        native_instance_key,
                                    )
                                    .map_err(Error::RendererError)?;
                            }
                        }
                        _ => {}
                    };
                } else {
                    unimplemented!();
                }
                Ok(())
            },
        };
        if let Some(current_tree) = self.current_tree.as_ref() {
            current_tree.walk_diff(&tree, &mut walker)?;
        } else {
            Tree::empty().walk_diff(&tree, &mut walker)?;
        }
        self.current_tree = Some(tree);
        Ok(())
    }

    pub async fn run(&mut self) {
        debug!("running");
        loop {
            tokio::select! {
                Some(event) = self.events_rx.recv() => {
                    // debug!("event: {:?}", event);
                match event {
                    Event::Render {
                        mut tree,
                        element,
                    } => {
                        self.commit(tree);
                    },
                }
                }
                Some(event) = self.component_events_rx.recv() => {
                    unimplemented!();
                }
            };
        }
    }

    pub fn render(&mut self, element: &Element<P>) -> Result<(), R::Error> {
        self.renderer
            .borrow_mut()
            .schedule_local_task(
                TaskPriority::Immediate,
                Box::new(RenderTask::<P, R>::new(
                    self.events_tx.clone(),
                    self.component_events_tx.clone(),
                    element.clone(),
                )),
            )
            .unwrap();
        Ok(())
    }
}

fn build_tree<'a, P, R>(
    nodes: &mut Arena<Node<P, R>>,
    fibers: &mut Arena<Fiber<P, R>>,
    element: &Element<P>,
) -> Result<NodeIndex, R::Error>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    // debug!("rendering a {:?}\n\n", element);
    let children = element.children();
    let fiber = match element {
        Element::Text(txt) => {
            let fiber = Fiber::text(txt.to_owned());
            Ok(fiber)
        }
        Element::Component(comp_element) => {
            let instance = comp_element.construct().map_err(Error::Sorcery)?;
            let fiber = Fiber::component(instance, comp_element.clone_props(), children.to_vec());
            Result::<_, R::Error>::Ok(fiber)
        }
        Element::Native(native) => {
            let instance = native.ty.clone();
            let fiber = Fiber::native(instance, native.props.clone(), children.to_vec());
            Ok(fiber)
        }
    }?;
    let fiber_index = FiberIndex(fibers.insert(fiber));
    let node = Node::new(fiber_index);
    let node_index = NodeIndex(nodes.insert(node));
    {
        let to_child = {
            let fiber = fibers.get_mut(*fiber_index).unwrap();
            fiber.node_index = Some(node_index);
            fiber
                .render()
                .map_err(Error::Sorcery)?
                .into_iter()
                .rev()
                .fold(Result::<_, R::Error>::Ok(None), |prev, next| {
                    let child_index = build_tree(nodes, fibers, &next)?;
                    let mut child = nodes.get_mut(*child_index).unwrap();
                    child.parent = Some(node_index);
                    child.sibling = prev?;
                    Ok(Some(child_index))
                })?
        };
        let mut node = nodes.get_mut(*node_index).unwrap();
        node.child = to_child;
    }
    Ok(node_index)
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

fn create_root<P, R>(
    nodes: &mut Arena<Node<P, R>>,
    fibers: &mut Arena<Fiber<P, R>>,
) -> (NodeIndex, FiberIndex)
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    let mut root_fiber = Fiber::root();
    let root_fiber_index = FiberIndex(fibers.insert(root_fiber));
    let mut root_node = Node::new(root_fiber_index);
    let root_node_index = NodeIndex(nodes.insert(root_node));
    fibers.get_mut(*root_fiber_index).unwrap().node_index = Some(root_node_index);
    (root_node_index, root_fiber_index)
}

#[async_trait(?Send)]
impl<P, R> LocalTask for RenderTask<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    async fn run(self: Box<Self>) -> TaskResult<()> {
        // debug!("rendering {:?}", self.root);
        let mut nodes = Arena::<Node<P, R>>::new();
        let mut fibers = Arena::<Fiber<P, R>>::new();
        let tree = {
            let node_index = build_tree(&mut nodes, &mut fibers, &self.root)?;
            let (root_node_index, root_fiber_index) = create_root(&mut nodes, &mut fibers);
            let root_node = nodes.get_mut(*root_node_index).unwrap();
            root_node.child = Some(node_index);
            nodes.get_mut(*node_index).unwrap().parent = Some(root_node_index);
            Tree::new(nodes, root_node_index, fibers, root_fiber_index)
        };
        self.tx
            .send(Event::Render {
                tree,
                element: self.root,
            })
            .unwrap();
        Ok(())
    }
}

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

pub trait Renderer<P>
where
    // TODO <P> should probably be an associated type (Primitive: RenderPrimitive)
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
    fn schedule_task(
        &self,
        priority: TaskPriority,
        task: Box<dyn Task>,
    ) -> std::result::Result<(), Self::Error>;

    fn schedule_local_task(
        &self,
        priority: TaskPriority,
        task: Box<dyn LocalTask>,
    ) -> std::result::Result<(), Self::Error>;
}

#[derive(Debug, Copy, Clone)]
pub enum TaskPriority {
    Immediate,
    Idle,
}

#[async_trait(?Send)]
pub trait LocalTask {
    async fn run(self: Box<Self>) -> TaskResult<()>;
}

#[async_trait]
pub trait Task {
    async fn run(self: Box<Self>) -> TaskResult<()>;
}

#[async_trait(?Send)]
impl<T> LocalTask for T
where
    T: Task,
{
    async fn run(self: Box<Self>) -> TaskResult<()> {
        LocalTask::run(self).await?;
        Ok(())
    }
}
