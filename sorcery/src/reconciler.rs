use crate::{
    AnyComponent, Component, ComponentElement, ComponentId, Dep, Element, Key, NativeElement,
    RenderPrimitive, StoredProps, StoredState,
};
use async_trait::async_trait;
use bumpalo::Bump;
use crossbeam_channel as channel;
use derivative::Derivative;
use generational_arena::{Arena, Index};
use std::{
    any::{Any, TypeId},
    cell::{Cell, RefCell},
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

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    nodes: Arena<Node<P, R>>,
    root_node_index: NodeIndex,
    fibers: Arena<Fiber<P, R>>,
    root_fiber_index: FiberIndex,
}

impl<P, R> fmt::Debug for Tree<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P> + 'static,
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
    R: Renderer<P> + 'static,
{
    fn empty(tx: mpsc::UnboundedSender<ComponentUpdate>) -> Self {
        let mut nodes = Arena::new();
        let mut fibers = Arena::new();
        let (root_node_index, root_fiber_index) = create_root(tx, &mut nodes, &mut fibers);
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

    fn fiber(&self, index: FiberIndex) -> Option<&Fiber<P, R>> {
        self.fibers.get(*index)
    }

    fn fiber_mut(&mut self, index: FiberIndex) -> Option<&mut Fiber<P, R>> {
        self.fibers.get_mut(*index)
    }

    fn node(&self, index: NodeIndex) -> Option<&Node<P, R>> {
        self.nodes.get(*index)
    }

    fn node_mut(&mut self, index: NodeIndex) -> Option<&mut Node<P, R>> {
        self.nodes.get_mut(*index)
    }

    fn root_fiber(&self) -> &Fiber<P, R> {
        self.fibers.get(*self.root_fiber_index).unwrap()
    }

    fn root_fiber_mut(&mut self) -> &mut Fiber<P, R> {
        self.fibers.get_mut(*self.root_fiber_index).unwrap()
    }

    fn child_fibers(&self, fiber: &Fiber<P, R>) -> Vec<&Fiber<P, R>> {
        self.child_fiber_ids(fiber)
            .into_iter()
            .filter_map(|id| self.fibers.get(*id))
            .collect()
    }

    fn child_fiber_ids(&self, fiber: &Fiber<P, R>) -> Vec<FiberIndex> {
        if let Some(node_index) = fiber.node_index {
            self.nodes
                .get(*node_index)
                .map(|node| node.children(&self.nodes))
                .unwrap_or(vec![])
                .into_iter()
                .filter_map(|node_index| self.nodes.get(*node_index).map(|node| node.fiber))
                .collect()
        } else {
            vec![]
        }
    }

    fn native_parent(&self, fiber: &Fiber<P, R>) -> Option<&Fiber<P, R>> {
        fiber.native_parent(&self.nodes, &self.fibers)
    }

    fn walk_diff<RF, AF, UF>(
        &self,
        other: &Self,
        walker: &mut DiffWalker<P, R, RF, AF, UF>,
    ) -> Result<(), R::Error>
    where
        RF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
        AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
        UF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    {
        fn walk_fiber<'t, P, R, RF, AF, UF>(
            left: &Tree<P, R>,
            left_fiber_index: Option<&'t Fiber<P, R>>,
            right: &'t Tree<P, R>,
            right_fiber: Option<&'t Fiber<P, R>>,
            walker: &mut DiffWalker<P, R, RF, AF, UF>,
        ) -> Result<(), R::Error>
        where
            P: RenderPrimitive,
            R: Renderer<P> + 'static,
            RF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
            AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
            UF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
        {
            // debug!(
            //     "comparing {:?} vs {:?} ======= {:?}",
            //     left_fiber,
            //     right_fiber,
            //     left_fiber == right_fiber
            // );
            match (left_fiber_index, right_fiber) {
                (Some(left_fiber), Some(right_fiber)) => {
                    // debug!("comparing {:?} vs {:?}", left_fiber.body, right_fiber.body);
                    if left_fiber != right_fiber {
                        if left_fiber.can_update_with(right_fiber) {
                            (walker.update)(left_fiber, right_fiber, right)?;
                        } else {
                            debug!("CANANOT UPDAATE THIS");
                        }
                    } else {
                        debug!("no need to update {:?}", left_fiber.body);
                    }

                    let left_children = left.child_fibers(left_fiber);
                    let right_children = right.child_fibers(right_fiber);
                    // debug!("{:?} -> {:?}", left_children, right_children);
                    match (left_children, right_children) {
                        (left_children, right_children)
                            if left_children.len() == right_children.len() =>
                        {
                            let len = left_children.len();
                            // debug!("updating all children ({:?})", len);
                            for index in 0..len {
                                walk_fiber(
                                    left,
                                    Some(left_children[index]),
                                    right,
                                    Some(right_children[index]),
                                    walker,
                                )?;
                            }
                        }
                        (left_children, right_children) if left_children.len() == 0 => {
                            // debug!("appending new children");
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
                    };
                }
                (None, Some(right_fiber)) => {
                    debug!("appending new child");
                    for child in right.child_fibers(right_fiber) {
                        walk_fiber(left, None, right, Some(child), walker)?;
                    }
                    (walker.append)(right_fiber, right)?;
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

struct DiffWalker<P, R, RF, AF, UF>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    RF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    UF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
{
    replace: RF,
    append: AF,
    update: UF,
    p: PhantomData<P>,
    r: PhantomData<R>,
}

impl<P, R, RF, AF, UF> DiffWalker<P, R, RF, AF, UF>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    RF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    AF: FnMut(&Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
    UF: FnMut(&Fiber<P, R>, &Fiber<P, R>, &Tree<P, R>) -> Result<(), R::Error>,
{
    fn new(replace: RF, append: AF, update: UF) -> Self {
        Self {
            p: PhantomData,
            r: PhantomData,
            replace,
            append,
            update,
        }
    }
}

pub struct Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    container: Arc<RefCell<R::Container>>,
    renderer: Arc<RefCell<R>>,
    events_rx: mpsc::UnboundedReceiver<Event<P, R>>,
    events_tx: mpsc::UnboundedSender<Event<P, R>>,
    component_events_rx: mpsc::UnboundedReceiver<ComponentUpdate>,
    component_events_tx: mpsc::UnboundedSender<ComponentUpdate>,
    current_tree: Option<Tree<P, R>>,
    // current_tree: bumpalo::collections::Vec<Fiber<P, R>>,
    // new_tree: bumpalo::collections::Vec<Fiber<P, R>>
}

#[derive(Debug)]
enum ComponentUpdate {
    SetState {
        pointer: usize,
        fiber_index: FiberIndex,
        value: Box<dyn StoredState>,
    },
    InitializeState {
        pointer: usize,
        fiber_index: FiberIndex,
        value: Box<dyn StoredState>,
    },
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
    R: Renderer<P> + 'static,
{
    Render {
        tree: Tree<P, R>,
        element: Element<P>,
    },
    Rerender {
        tree: Tree<P, R>,
    },
    SetState {
        pointer: usize,
        fiber_index: FiberIndex,
        value: Box<dyn StoredState>,
    },
    InitializeState {
        pointer: usize,
        fiber_index: FiberIndex,
        value: Box<dyn StoredState>,
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
    tx: mpsc::UnboundedSender<ComponentUpdate>,
    // props: Box<dyn StoredProps>,
    // state: Box<dyn StoredState>,
    // children: Vec<Element<P>>,
    dirty: bool,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
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

    fn can_update_with(&self, other: &Self) -> bool {
        match (&self.body, &other.body) {
            (Some(body), Some(other_body)) => body.can_update_with(other_body),
            _ => false,
        }
    }

    fn is_root(&self) -> bool {
        match &self.body {
            Some(FiberBody::Root) => true,
            _ => false,
        }
    }

    fn update_state(&mut self, pointer: usize, state: Box<dyn StoredState>) {
        if let Some(mut body) = self.body.as_mut() {
            body.update_state(pointer, state);
        }
    }

    fn init_state(&mut self, pointer: usize, state: Box<dyn StoredState>) {
        if let Some(mut body) = self.body.as_mut() {
            body.init_state(pointer, state);
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

    fn fiber_mut<'a>(&self, arena: &'a mut Arena<Fiber<P, R>>) -> Option<&'a mut Fiber<P, R>> {
        arena.get_mut(*self.fiber)
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
    fn new(tx: mpsc::UnboundedSender<ComponentUpdate>, body: FiberBody<P, R>) -> Self {
        Self {
            id: FiberId::gen(),
            node_index: None,
            dirty: false,
            body: Some(body),
            tx,
        }
    }

    fn root(tx: mpsc::UnboundedSender<ComponentUpdate>) -> Self {
        Self::new(tx, FiberBody::Root)
    }

    fn text(tx: mpsc::UnboundedSender<ComponentUpdate>, text: String) -> Self {
        Self::new(tx, FiberBody::Text(text, None))
    }

    fn component(
        tx: mpsc::UnboundedSender<ComponentUpdate>,
        instance: Box<dyn AnyComponent<P>>,
        props: Box<dyn StoredProps>,
        children: Vec<Element<P>>,
    ) -> Self {
        let body = FiberBody::Component {
            state: vec![],
            instance,
            props,
            children,
        };
        Self::new(tx, body)
    }

    fn native(
        tx: mpsc::UnboundedSender<ComponentUpdate>,
        instance: P,
        props: P::Props,
        children: Vec<Element<P>>,
    ) -> Self {
        let body = FiberBody::Native {
            instance,
            props,
            native_instance_key: None,
            children,
        };
        Self::new(tx, body)
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

    fn render(&mut self, fiber_index: FiberIndex) -> crate::Result<Vec<Element<P>>> {
        let children = match &self.body {
            Some(FiberBody::Root) => {
                unimplemented!("root");
            }
            Some(FiberBody::Text(_, _)) => Ok(vec![]),
            Some(FiberBody::Component {
                instance,
                props,
                state,
                ..
            }) => {
                let mut context = RenderContext {
                    state_pointer: Cell::new(0),
                    state: state.clone(),
                    component_tx: self.tx.clone(),
                    fiber_index,
                };
                let children = self.children().unwrap_or(&[]);
                Ok(vec![instance.render(
                    &mut context,
                    props.as_ref(),
                    children,
                )?])
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
// TODO(cab) instead of using context, we should wrap AnyComponent in an "Instance"
#[derive(Debug)]
pub struct RenderContext {
    state_pointer: Cell<usize>,
    state: Vec<Box<dyn StoredState>>,
    fiber_index: FiberIndex,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
}

#[derive(Debug)]
enum RenderOp {}

impl RenderContext {
    fn current_state(&self) -> Option<&Box<dyn StoredState>> {
        debug!("check state {:?}", self.state);
        self.state.get(self.state_pointer.get())
    }

    fn increment_pointer(&self) {
        self.state_pointer.set(self.state_pointer.get() + 1);
    }

    fn pointer(&self) -> usize {
        self.state_pointer.get()
    }

    pub fn use_state<'i, T>(&'i self, initial: &'i T) -> (&'i T, impl Fn(T) + Clone + 'static)
    where
        T: StoredState + Clone + 'static,
    {
        let tx = self.component_tx.clone();
        let pointer = self.pointer();
        let fiber_index = self.fiber_index;
        let setter = {
            let tx = tx.clone();
            move |new_state: T| {
                tx.send(ComponentUpdate::SetState {
                    pointer,
                    fiber_index,
                    value: Box::new(new_state),
                })
                .unwrap();
            }
        };
        let current_value = if let Some(state) = self.current_state() {
            Any::downcast_ref::<T>(state.as_ref().as_any()).unwrap() // todo
        } else {
            tx.send(ComponentUpdate::InitializeState {
                fiber_index,
                pointer,
                value: Box::new(initial.clone()),
            })
            .unwrap();
            initial
        };
        self.increment_pointer();
        (current_value, setter)
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
        state: Vec<Box<dyn StoredState>>,
        children: Vec<Element<P>>,
    },
    Native {
        instance: P,
        native_instance_key: Option<R::InstanceKey>,
        props: P::Props,
        children: Vec<Element<P>>,
    },
}

impl<P, R> FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn can_update_with(&self, other: &Self) -> bool {
        match (&self, other) {
            (FiberBody::Root, FiberBody::Root) => true,
            (FiberBody::Text(_, _), FiberBody::Text(_, _)) => true,
            (
                FiberBody::Component { instance, .. },
                FiberBody::Component {
                    instance: other_instance,
                    ..
                },
            ) => instance.kind_id() == other_instance.kind_id(),
            (
                FiberBody::Native { instance, .. },
                FiberBody::Native {
                    instance: other_instance,
                    ..
                },
            ) => instance == other_instance,
            _ => false,
        }
    }

    fn update_state(&mut self, pointer: usize, new_state: Box<dyn StoredState>) {
        match self {
            FiberBody::Component { ref mut state, .. } => {
                if pointer > state.len() {
                    panic!("bad state pointer");
                    return;
                }
                state[pointer] = new_state;
            }
            _ => {
                error!("cannot update state for non-component");
            }
        };
    }

    fn init_state(&mut self, pointer: usize, new_state: Box<dyn StoredState>) {
        match self {
            FiberBody::Component { ref mut state, .. } => {
                state.push(new_state);
                if state.len() - 1 != pointer {
                    panic!(
                        "bad state set TODO ({} vs pointer={}) [{:?}]",
                        state.len() - 1,
                        pointer,
                        state
                    );
                }
            }
            _ => {
                error!("cannot init state for non-component");
            }
        };
    }
}

impl<P, R> PartialEq<FiberBody<P, R>> for FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn eq(&self, other: &FiberBody<P, R>) -> bool {
        // warn!("IMPLEMENT PARTIALEQ FOR REAL");
        match (self, other) {
            (FiberBody::Root, FiberBody::Root) => {
                // roots are always equal
                true
            }
            (FiberBody::Text(t, _), FiberBody::Text(t2, _)) => t == t2,
            (
                FiberBody::Component {
                    children,
                    props,
                    state,
                    ..
                },
                FiberBody::Component {
                    children: children2,
                    props: props2,
                    state: other_state,
                    ..
                },
            ) => {
                // TODO(cab) is a state len check enough?
                children == children2 && props == props2 && state.len() == other_state.len()
            }

            (
                FiberBody::Native {
                    instance, props, ..
                },
                FiberBody::Native {
                    instance: instance2,
                    props: props2,
                    ..
                },
            ) => props == props2 && instance == instance2,
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
            container: Arc::new(RefCell::new(container)),
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
                    let debug = InstanceDebug {
                        id: fiber.id.0.to_string(),
                    };
                    *native_instance_key = Some(
                        self.renderer
                            .borrow_mut()
                            .create_instance(instance, props, &debug)
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
        let renderer = self.renderer.clone();
        let mut walker = DiffWalker::<P, _, _, _, _>::new(
            {
                let renderer = renderer.clone();
                let container = self.container.clone();
                move |old, new, old_tree, new_tree| {
                    // move node
                    if let Some(parent) = old_tree.native_parent(old) {
                        debug!("fyi parent is {:?}", parent.body);

                        match (&old.body, &new.body) {
                            (
                                Some(FiberBody::Text(text, Some(text_instance_key))),
                                Some(FiberBody::Text(text2, _)),
                            ) => {
                                if parent.is_root() {
                                    unimplemented!("replace root text");
                                } else {
                                    warn!("replace parent text {:?} -> {:?}", text, text2);
                                }
                            }
                            (
                                Some(FiberBody::Native {
                                    native_instance_key: Some(native_instance_key_old),
                                    ..
                                }),
                                Some(FiberBody::Native {
                                    native_instance_key: Some(native_instance_key_new),
                                    ..
                                }),
                            ) => {
                                if parent.is_root() {
                                    debug!(
                                        "replace root + native ({:?} -> {:?})",
                                        native_instance_key_old, native_instance_key_new
                                    );
                                    renderer
                                        .borrow_mut()
                                        .insert_child_in_container_before(
                                            &mut container.borrow_mut(),
                                            native_instance_key_new,
                                            native_instance_key_old,
                                        )
                                        .map_err(Error::RendererError)?;
                                    renderer
                                        .borrow_mut()
                                        .remove_child_from_container(
                                            &mut container.borrow_mut(),
                                            native_instance_key_old,
                                        )
                                        .map_err(Error::RendererError)?;
                                } else {
                                    debug!(
                                        "replace root + parent ({:?} -> {:?})",
                                        native_instance_key_old, native_instance_key_new
                                    );
                                    if native_instance_key_new == native_instance_key_old {
                                        panic!();
                                    }
                                    renderer
                                        .borrow_mut()
                                        .insert_child_in_parent_before(
                                            parent.native_instance_key().unwrap(),
                                            native_instance_key_new,
                                            native_instance_key_old,
                                        )
                                        .map_err(Error::RendererError)?;
                                    renderer
                                        .borrow_mut()
                                        .remove_child_from_parent(
                                            parent.native_instance_key().unwrap(),
                                            native_instance_key_old,
                                        )
                                        .map_err(Error::RendererError)?;
                                }
                            }
                            _ => {}
                        };
                    } else {
                        unimplemented!("missing parent");
                    }

                    Ok(())
                }
            },
            {
                let renderer = renderer.clone();
                let container = self.container.clone();
                move |child, tree| {
                    if let Some(parent) = tree.native_parent(child) {
                        debug!("append {:?} to {:?}", child.body, parent.body);
                        match &child.body {
                            Some(FiberBody::Text(text, Some(text_instance_key))) => {
                                if parent.is_root() {
                                    unimplemented!("root text");
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
                                        .append_child_to_container(
                                            &mut container.borrow_mut(),
                                            native_instance_key,
                                        )
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
                            other => {
                                debug!("SKIPPED APPEND!!!!!");
                            }
                        };
                    } else {
                        unimplemented!("missing parent");
                    }
                    Ok(())
                }
            },
            {
                let renderer = renderer.clone();
                move |fiber, with, tree| {
                    debug!("update {:?} to match {:?}", fiber, with);
                    match (fiber.body.as_ref().unwrap(), with.body.as_ref().unwrap()) {
                        (FiberBody::Root, FiberBody::Root) => {
                            // skip
                        }
                        (FiberBody::Component { .. }, FiberBody::Component { .. }) => {}
                        (
                            FiberBody::Native {
                                native_instance_key: Some(native_instance_key),
                                ..
                            },
                            FiberBody::Native {
                                props: new_props, ..
                            },
                        ) => {
                            renderer
                                .borrow_mut()
                                .update_instance_props(
                                    native_instance_key,
                                    P::Props::clone(new_props),
                                )
                                .map_err(Error::RendererError)?;
                        }
                        (FiberBody::Text(_, Some(instance)), FiberBody::Text(text, _)) => {
                            renderer
                                .borrow_mut()
                                .update_text(instance, text)
                                .map_err(Error::RendererError)?;
                        }
                        (a, b) => {
                            unimplemented!("{:?} with {:?}", a, b);
                        }
                    }
                    Ok(())
                }
            },
        );
        if let Some(current_tree) = self.current_tree.as_mut() {
            current_tree.walk_diff(&tree, &mut walker)?;
        } else {
            Tree::empty(self.component_events_tx.clone()).walk_diff(&tree, &mut walker)?;
        }
        self.current_tree = Some(tree);

        Ok(())
    }

    pub async fn run(&mut self) {
        debug!("running");
        let mut current_element = None;
        loop {
            tokio::select! {
                Some(event) = self.component_events_rx.recv() => {
                    debug!("component event: {:?}", event);
                    match event {
                        ComponentUpdate::SetState {
                            pointer, fiber_index,
                            value
                        } => {
                            self.events_tx.send(Event::SetState {
                                pointer, fiber_index, value
                            }).unwrap();
                        },
                        ComponentUpdate::InitializeState {pointer, value, fiber_index} => {
                            self.events_tx.send(Event::InitializeState { pointer, value, fiber_index}).unwrap();
                        }
                    };

                }
                Some(event) = self.events_rx.recv() => {
                    // debug!("event: {:?}", event);
                match event {
                    Event::Render {
                        mut tree,
                        element,
                    } => {
                        self.commit(tree).unwrap();
                        current_element = Some(element);
                    },
                    Event::Rerender { mut tree } => {
                        self.commit(tree).unwrap();
                    },
                    Event::SetState { pointer, fiber_index, value} => {
                        if let Some(mut fiber) = self.current_tree.as_mut().unwrap().fiber_mut(fiber_index) {
                            debug!("UPDATING STATE");
                            fiber.update_state(pointer, value);
                            self.rerender(fiber_index).unwrap();
                        } else {
                            panic!("invalid state set");
                        }
                    },
                    Event::InitializeState {pointer, value, fiber_index} => {
                        if let Some(mut fiber) = self.current_tree.as_mut().unwrap().fiber_mut(fiber_index) {
                            fiber.init_state(pointer, value);
                        }
                    }
                }
                }

            };
        }
    }

    fn rerender(&mut self, fiber: FiberIndex) -> Result<(), R::Error> {
        self.renderer
            .borrow_mut()
            .schedule_local_task(
                TaskPriority::Immediate,
                Box::new(UpdateFiberTask::<P, R>::new(
                    self.events_tx.clone(),
                    self.component_events_tx.clone(),
                    fiber,
                    self.current_tree.clone().unwrap(),
                )),
            )
            .unwrap();
        Ok(())
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
    tx: &mpsc::UnboundedSender<ComponentUpdate>,
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
            let fiber = Fiber::text(tx.clone(), txt.to_owned());
            Ok(fiber)
        }
        Element::Component(comp_element) => {
            let instance = comp_element.construct().map_err(Error::Sorcery)?;
            let fiber = Fiber::component(
                tx.clone(),
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
                instance,
                native.props.clone(),
                children.to_vec(),
            );
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
                .render(fiber_index)
                .map_err(Error::Sorcery)?
                .into_iter()
                .rev()
                .fold(Result::<_, R::Error>::Ok(None), |prev, next| {
                    let child_index = build_tree(tx, nodes, fibers, &next)?;
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
    R: Renderer<P> + 'static,
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
    tx: mpsc::UnboundedSender<ComponentUpdate>,
    nodes: &mut Arena<Node<P, R>>,
    fibers: &mut Arena<Fiber<P, R>>,
) -> (NodeIndex, FiberIndex)
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    let mut root_fiber = Fiber::root(tx);
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
            let node_index = build_tree(&self.component_tx, &mut nodes, &mut fibers, &self.root)?;
            let (root_node_index, root_fiber_index) =
                create_root(self.component_tx.clone(), &mut nodes, &mut fibers);
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

struct UpdateFiberTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P> + 'static,
{
    fiber: FiberIndex,
    tree: Tree<P, R>,
    tx: mpsc::UnboundedSender<Event<P, R>>,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
}

impl<P, R> UpdateFiberTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        tx: mpsc::UnboundedSender<Event<P, R>>,
        component_tx: mpsc::UnboundedSender<ComponentUpdate>,
        fiber: FiberIndex,
        tree: Tree<P, R>,
    ) -> Self {
        Self {
            fiber,
            tree,
            tx,
            component_tx,
        }
    }
}

#[async_trait(?Send)]
impl<P, R> LocalTask for UpdateFiberTask<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    async fn run(mut self: Box<Self>) -> TaskResult<()> {
        // debug!("rendering {:?}", self.root);
        let mut tree = self.tree;
        let (node_index, new_children) = {
            let fiber = tree.fiber_mut(self.fiber).unwrap(); // todo
            (fiber.node_index.unwrap(), fiber.render(self.fiber)?)
        };
        let to_child = {
            let nodes = &mut tree.nodes;
            let fibers = &mut tree.fibers;
            let tx = self.component_tx.clone();
            new_children.into_iter().rev().fold(
                Result::<_, R::Error>::Ok(None),
                move |prev, next| {
                    let child_node_index = build_tree(&tx, nodes, fibers, &next)?;
                    let mut child = nodes.get_mut(*child_node_index).unwrap();
                    child.parent = Some(node_index);
                    child.sibling = prev?;
                    Ok(Some(child_node_index))
                },
            )?
        };
        let node = tree.node_mut(node_index).unwrap();
        node.child = to_child;
        self.tx.send(Event::Rerender { tree: tree }).unwrap();
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

#[derive(Debug)]
pub struct InstanceDebug {
    pub id: String,
}

pub trait Renderer<P>
where
    // TODO <P> should probably be an associated type (Primitive: RenderPrimitive)
    P: RenderPrimitive,
{
    type Error: std::error::Error + 'static;
    type Container;
    type InstanceKey: PartialEq + Clone + std::fmt::Debug;
    type TextInstanceKey: Clone + std::fmt::Debug;
    fn create_instance(
        &mut self,
        ty: &P,
        props: &P::Props,
        debug: &InstanceDebug,
    ) -> std::result::Result<Self::InstanceKey, Self::Error>;
    fn create_text_instance(
        &mut self,
        text: &str,
    ) -> std::result::Result<Self::TextInstanceKey, Self::Error>;
    fn update_instance_props(
        &mut self,
        instance: &Self::InstanceKey,
        new_props: P::Props,
    ) -> std::result::Result<(), Self::Error>;
    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn insert_child_in_container_before<'r>(
        &mut self,
        container: &mut Self::Container,
        child: &Self::InstanceKey,
        before: &Self::InstanceKey,
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
    fn insert_child_in_parent_before<'r>(
        &mut self,
        parent: &Self::InstanceKey,
        child: &Self::InstanceKey,
        before: &Self::InstanceKey,
    ) -> std::result::Result<(), Self::Error>;
    fn update_text(
        &mut self,
        instance: &Self::TextInstanceKey,
        text: &str,
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
