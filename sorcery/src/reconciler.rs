use crate::{
    AnyComponent, Component, ComponentElement, ComponentId, Dep, Element, Key, NativeElement,
    RenderPrimitive, StoredProps, StoredState,
};
use async_trait::async_trait;
use bumpalo::Bump;
use crossbeam_channel as channel;
use generational_arena::{Arena, Index};
use std::{
    any::{Any, TypeId},
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, VecDeque},
    fmt,
    future::Future,
    marker::PhantomData,
    sync::Arc,
};
use tokio::sync::{mpsc, RwLock};
use tracing::{debug, error, trace, warn};

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
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    nodes: Arena<Node<P, R>>,
    root_node_index: NodeIndex,
    fibers: Arena<Fiber<P, R>>,
    root_fiber_index: FiberIndex,
}

impl<P, R> fmt::Debug for Tree<P, R>
where
    P: RenderPrimitive + 'static,
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
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    fn new(
        nodes: Arena<Node<P, R>>,
        root_node_index: NodeIndex,
        fibers: Arena<Fiber<P, R>>,
        root_fiber_index: FiberIndex,
    ) -> Self {
        Self {
            nodes,
            root_node_index,
            fibers,
            root_fiber_index,
        }
    }

    fn build(element: &Element<P>) -> Result<Self, R::Error> {
        let mut nodes = Arena::new();
        let mut fibers = Arena::new();
        let root_fiber = Fiber::root(vec![element.clone()]);
        let root_fiber_index = FiberIndex(fibers.insert(root_fiber));
        let root_node = Node::new(root_fiber_index);
        let root_node_index = NodeIndex(nodes.insert(root_node));
        let mut tree = Self::new(nodes, root_node_index, fibers, root_fiber_index);
        {
            tree.fiber_mut(root_fiber_index).unwrap().node_index = Some(root_node_index);
        }
        debug!("rendering into");
        tree.render_at(tree.root_node_index)?;
        tree.render_at(tree.root_node_index)?;
        tree.walk(|_, f, _, _| {
            debug!("wwalking, {:?}", f);
            Ok(())
        })?;
        Ok(tree)
    }

    fn render_element(
        &mut self,
        element: &Element<P>,
    ) -> Result<(FiberIndex, NodeIndex), R::Error> {
        // debug!("rendering a {:?}\n\n", element);
        let children = element.children();
        let fiber = match element {
            Element::Text(txt) => {
                let fiber = Fiber::text(txt.to_owned());
                Ok(fiber)
            }
            Element::Component(comp_element) => {
                let instance = comp_element.construct().map_err(Error::Sorcery)?;
                let fiber =
                    Fiber::component(instance, comp_element.clone_props(), children.to_vec());
                Result::<_, R::Error>::Ok(fiber)
            }
            Element::Native(native) => {
                let instance = native.ty.clone();
                let fiber = Fiber::native(instance, native.props.clone(), children.to_vec());
                Ok(fiber)
            }
        }?;
        let fiber_index = FiberIndex(self.fibers.insert(fiber));
        let node = Node::new(fiber_index);
        let node_index = NodeIndex(self.nodes.insert(node));

        {
            let fiber = self.fibers.get_mut(*fiber_index).unwrap();
            fiber.node_index = Some(node_index);
        }

        // {
        //     let to_child = {
        //         let fiber = self.fibers.get_mut(*fiber_index).unwrap();
        //         fiber.node_index = Some(node_index);
        //         fiber
        //             .render()
        //             .map_err(Error::Sorcery)?
        //             .into_iter()
        //             .rev()
        //             .fold(Result::<_, R::Error>::Ok(None), |prev, next| {
        //                 let child_index = self.render_element(&next)?;
        //                 let mut child = self.nodes.get_mut(*child_index).unwrap();
        //                 child.parent = Some(node_index);
        //                 child.sibling = prev?;
        //                 Ok(Some(child_index))
        //             })?
        //     };
        //     let mut node = self.nodes.get_mut(*node_index).unwrap();
        //     node.child = to_child;
        // }
        Ok((fiber_index, node_index))
    }

    fn render_at(&mut self, node_index: NodeIndex) -> Result<(), R::Error> {
        let first_child_index = if let Some(node) = self.node(node_index) {
            if let Some(fiber) = self.fiber(node.fiber) {
                debug!("render_at for {:?}", fiber.body);
                let existing_children = self.child_fibers(fiber);
                debug!("existing {:?}", existing_children);
                let first_child_index = fiber.render()?.iter().rev().fold(
                    Result::<_, R::Error>::Ok(None),
                    |prev, next| {
                        let (child_fiber_index, child_node_index) = self.render_element(next)?;
                        self.render_at(child_node_index)?;
                        let mut child = self.nodes.get_mut(*child_node_index).unwrap();
                        child.parent = Some(node_index);
                        child.sibling = prev?;
                        Ok(Some(child_node_index))
                    },
                )?;

                first_child_index
            } else {
                unimplemented!();
            }
        } else {
            unimplemented!();
        };

        if let Some(node) = self.node_mut(node_index) {
            node.child = first_child_index;
        }

        if let Some(node) = self.node(node_index) {
            if let Some(fiber) = self.fiber(node.fiber) {
                debug!("finished render_at for {:?}", fiber.body);
            }
        }
        Ok(())
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

    fn root_node(&self) -> &Node<P, R> {
        self.nodes.get(*self.root_node_index).unwrap()
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
}

pub struct Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    container: Arc<RefCell<R::Container>>,
    renderer: Arc<RefCell<R>>,
    current_tree: Option<Arc<Tree<P, R>>>,
    events_tx: mpsc::UnboundedSender<Event<P, R>>,
    events_rx: mpsc::UnboundedReceiver<Event<P, R>>,
}

struct Fiber<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    node_index: Option<NodeIndex>,
    body: Option<FiberBody<P, R>>,
    updates: Vec<FiberUpdate>,
    internal_events_tx: channel::Sender<FiberUpdate>,
    internal_events_rx: channel::Receiver<FiberUpdate>,
}

impl<P, R> fmt::Debug for Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fiber").field("body", &self.body).finish()
    }
}

#[derive(Debug)]
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
            Some(FiberBody::Root(_)) => true,
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
            Some(FiberBody::Root(_)) => true,
            _ => false,
        }
    }

    fn update_state(&mut self, pointer: usize, state: Box<dyn StoredState>) {
        debug!("updating state");
        if let Some(mut body) = self.body.as_mut() {
            body.update_state(pointer, state);
        } else {
            unimplemented!("bad update state");
        }
    }

    fn init_state(&mut self, pointer: usize, state: Box<dyn StoredState>) {
        debug!("initializing state");
        if let Some(mut body) = self.body.as_mut() {
            body.init_state(pointer, state);
        } else {
            unimplemented!("bad init state");
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

impl<'r, P, R> Fiber<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P>,
{
    fn new(body: FiberBody<P, R>) -> Self {
        let (internal_events_tx, internal_events_rx) = channel::unbounded();
        Self {
            node_index: None,
            updates: Vec::new(),
            body: Some(body),
            internal_events_rx,
            internal_events_tx,
        }
    }

    fn root(children: Vec<Element<P>>) -> Self {
        Self::new(FiberBody::Root(children))
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
            state: vec![],
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
            Some(FiberBody::Root(children)) => Some(&children),
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

    fn process_updates(&mut self) -> crate::Result<bool> {
        let mut updated = false;
        for update in self.internal_events_rx.try_iter().collect::<Vec<_>>() {
            match update {
                FiberUpdate::InitState { pointer, value } => {
                    self.init_state(pointer, value);
                    updated = true;
                }
                FiberUpdate::SetState { pointer, value } => {
                    self.update_state(pointer, value);
                    updated = true;
                }
            }
        }
        Ok(updated)
    }

    fn render(&self) -> crate::Result<Vec<Element<P>>> {
        let children = match &self.body {
            Some(FiberBody::Root(children)) => Ok(children.to_owned()),
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
                    new_state: RefCell::new(MutableState::new()),
                    internal_events_tx: self.internal_events_tx.clone(),
                    trigger_rerender: Arc::new(move || {}),
                };
                let children = self.children().unwrap_or(&[]);
                let rendered = instance.render(&mut context, props.as_ref(), children)?;
                let updates: Vec<_> = context.new_state.into_inner().into();
                for update in updates {
                    self.internal_events_tx.send(update).unwrap();
                }
                Ok(vec![rendered])
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

#[derive(Debug, Clone)]
enum FiberUpdate {
    SetState {
        pointer: usize,
        value: Box<dyn StoredState>,
    },
    InitState {
        pointer: usize,
        value: Box<dyn StoredState>,
    },
}

pub struct RenderContext {
    state_pointer: Cell<usize>,
    new_state: RefCell<MutableState>,
    state: Vec<Box<dyn StoredState>>,
    internal_events_tx: channel::Sender<FiberUpdate>,
    trigger_rerender: Arc<dyn Fn()>,
}

impl fmt::Debug for RenderContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RenderContext").finish()
    }
}

#[derive(Debug)]
struct MutableState {
    new_state: Vec<(usize, Box<dyn StoredState>)>,
}

impl Into<Vec<FiberUpdate>> for MutableState {
    fn into(self) -> Vec<FiberUpdate> {
        let mut updates = Vec::new();
        for (pointer, value) in self.new_state {
            updates.push(FiberUpdate::InitState { pointer, value })
        }
        updates
    }
}

impl MutableState {
    fn new() -> Self {
        Self {
            new_state: Vec::new(),
        }
    }

    fn create(&mut self, pointer: usize, value: Box<dyn StoredState>) {
        self.new_state.push((pointer, value));
    }
}

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

    fn insert_state(&self, state: Box<dyn StoredState>) {
        self.new_state.borrow_mut().create(self.pointer(), state);
    }

    pub fn use_state<'r, T>(&'r self, initial: &'r T) -> (&'r T, impl Fn(T) + Clone + 'static)
    where
        T: StoredState + Clone + 'static,
    {
        let pointer = self.pointer();
        let tx = self.internal_events_tx.clone();
        let rerender = self.trigger_rerender.clone();
        let setter = {
            move |new_state: T| {
                tx.send(FiberUpdate::SetState {
                    pointer,
                    value: Box::new(new_state),
                })
                .unwrap();
                rerender();
            }
        };
        let current_value = if let Some(state) = self.current_state() {
            Any::downcast_ref::<T>(state.as_ref().as_any()).unwrap() // todo
        } else {
            self.insert_state(Box::new(initial.clone()));
            initial
        };
        self.increment_pointer();
        (current_value, setter)
    }
}

enum FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    Root(Vec<Element<P>>),
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

impl<P, R> fmt::Debug for FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FiberBody::Component { instance, .. } => f
                .debug_struct("FiberBody::Component")
                .field("instance", instance)
                .finish(),
            FiberBody::Text(_, _) => f.debug_struct("FiberBody::Text").finish(),
            FiberBody::Native { .. } => f.debug_struct("FiberBody::Native").finish(),
            FiberBody::Root(_) => f.debug_struct("FiberBody::Root").finish(),
        }
    }
}

impl<P, R> FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn can_update_with(&self, other: &Self) -> bool {
        match (&self, other) {
            (FiberBody::Root(_), FiberBody::Root(_)) => true,
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
            (FiberBody::Root(_), FiberBody::Root(_)) => {
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

#[derive(Debug)]
enum Event<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    DiffTree { tree: Tree<P, R> },
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    pub fn new(renderer: R, container: R::Container) -> Self {
        let (events_tx, events_rx) = mpsc::unbounded_channel();
        Self {
            container: Arc::new(RefCell::new(container)),
            renderer: Arc::new(RefCell::new(renderer)),
            current_tree: None,
            events_tx,
            events_rx,
        }
    }

    pub async fn run(&mut self) {
        debug!("running");
        while let Some(event) = self.events_rx.recv().await {
            // debug!("event: {:?}", event);
            match event {
                Event::DiffTree { mut tree } => {
                    unimplemented!();
                }
            }
        }
    }

    pub fn render(&mut self, element: &Element<P>) -> Result<(), R::Error> {
        self.renderer
            .borrow_mut()
            .schedule_local_task(
                TaskPriority::Immediate,
                Box::new(RenderTask::<P, R>::new(
                    self.events_tx.clone(),
                    element.clone(),
                    self.current_tree.clone(),
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
    P: RenderPrimitive + 'static,
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
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    tx: mpsc::UnboundedSender<Event<P, R>>,
    existing_tree: Option<Arc<Tree<P, R>>>,
    root: Element<P>,
}

trait ElementExt<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn create_fiber(&self, children: &[Element<P>]) -> Result<Fiber<P, R>, R::Error>;
}

impl<P, R> ElementExt<P, R> for Element<P>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn create_fiber(&self, children: &[Element<P>]) -> Result<Fiber<P, R>, R::Error> {
        let fiber = match self {
            Element::Text(txt) => {
                let fiber = Fiber::text(txt.to_owned());
                Ok(fiber)
            }
            Element::Component(comp_element) => {
                let instance = comp_element.construct().map_err(Error::Sorcery)?;
                let fiber =
                    Fiber::component(instance, comp_element.clone_props(), children.to_vec());
                Result::<_, R::Error>::Ok(fiber)
            }
            Element::Native(native) => {
                let instance = native.ty.clone();
                let fiber = Fiber::native(instance, native.props.clone(), children.to_vec());
                Ok(fiber)
            }
        }?;
        Ok(fiber)
    }
}

impl<P, R> RenderTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn new(
        tx: mpsc::UnboundedSender<Event<P, R>>,
        root: Element<P>,
        existing_tree: Option<Arc<Tree<P, R>>>,
    ) -> Self {
        Self {
            root,
            tx,
            existing_tree,
        }
    }
}

#[async_trait(?Send)]
impl<P, R> LocalTask for RenderTask<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    async fn run(self: Box<Self>) -> TaskResult<()> {
        if let Some(current_tree) = self.existing_tree {
            unimplemented!();
        } else {
            debug!("building new tree");
            let tree: Tree<P, R> = Tree::build(&self.root)?;
        }
        Ok(())
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
