use crossbeam_channel as channel;
use derivative::Derivative;
use generational_arena::{Arena, Index as ArenaNodeId};
use sorcery::{
    AnyComponent, Component, ComponentContext, ComponentElement, ComponentId, ComponentUpdate, Dep,
    Element, Key, NativeElement, RenderPrimitive, StoredProps, StoredState,
};
use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    marker::PhantomData,
    sync::Arc,
};
use tokio::sync::mpsc;
use tracing::{debug, trace, warn};

#[derive(thiserror::Error, Debug)]
pub enum Error<E>
where
    E: std::error::Error + 'static,
{
    #[error("renderer error")]
    RendererError(E),
    #[error(transparent)]
    Sorcery(#[from] sorcery::Error),
}

pub type Result<T, E> = std::result::Result<T, Error<E>>;

pub struct Reconciler<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    next_fiber_id: u32,
    element_type: std::marker::PhantomData<P>,
    renderer: Arc<RefCell<R>>,
    events_rx: mpsc::UnboundedReceiver<Event<P, R>>,
    events_tx: mpsc::UnboundedSender<Event<P, R>>,
    component_events_rx: mpsc::UnboundedReceiver<ComponentUpdate>,
    component_events_tx: mpsc::UnboundedSender<ComponentUpdate>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum Event<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    InitialRender {
        fibers: Arena<Fiber<P, R>>,
        root_id: ArenaNodeId,
        #[derivative(Debug = "ignore")]
        container: R::Container,
    },
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Hash, Eq)]
struct FiberId(u32);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    id: FiberId,
    context: sorcery::ComponentContext,
    body: Option<FiberBody<P, R>>,
    sibling: Option<ArenaNodeId>,
    child: Option<ArenaNodeId>,
    parent: Option<ArenaNodeId>,
}

impl<P, R> Fiber<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    fn parent<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.parent.and_then(|p| arena.get(p))
    }

    fn sibling<'a>(&self, arena: &'a Arena<Self>) -> Option<&'a Self> {
        self.sibling.and_then(|p| arena.get(p))
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
        arena.get_mut(id)
    }

    fn parent_native_id<'a>(&'a self, arena: &'a Arena<Self>) -> Option<ArenaNodeId> {
        let mut parent = self.parent;
        while let Some(p) = parent {
            if let Some(node) = arena.get(p) {
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
    fn text(tx: mpsc::UnboundedSender<ComponentUpdate>, id: FiberId, text: String) -> Self {
        Self {
            context: sorcery::ComponentContext::new(tx, FiberComponentContext::new(id)),
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(FiberBody::Text(text, None)),
        }
    }

    fn component(
        tx: mpsc::UnboundedSender<ComponentUpdate>,
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
            context: sorcery::ComponentContext::new(tx, FiberComponentContext::new(id)),
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(body),
        }
    }

    fn native(
        tx: mpsc::UnboundedSender<ComponentUpdate>,
        id: FiberId,
        instance: P,
        props: P::Props,
    ) -> Self {
        let body = FiberBody::Native {
            instance,
            props,
            native_instance_key: None,
        };
        let state = Box::new(());
        Self {
            context: sorcery::ComponentContext::new(tx, FiberComponentContext::new(id)),
            id,
            sibling: None,
            child: None,
            parent: None,
            body: Some(body),
        }
    }

    fn render(&mut self, children: &[Element<P>]) -> sorcery::Result<Vec<Element<P>>> {
        // let child_elements = self
        //     .children
        //     .iter_mut()
        //     .map(|fiber| fiber.render())
        //     .collect::<Result<Vec<_>>>()?
        //     .into_iter()
        //     .filter_map(|f| f)
        //     .collect::<Vec<_
        self.context.reset();
        let children = match &self.body {
            Some(FiberBody::Root) => {
                unimplemented!("root");
            }
            Some(FiberBody::Text(_, _)) => Ok(vec![]),
            Some(FiberBody::Component {
                instance, props, ..
            }) => Ok(vec![instance.render(
                &mut self.context,
                props.as_ref(),
                children,
            )?]),
            Some(FiberBody::Native {
                instance, props, ..
            }) => Ok(instance.render(&props, children)?),
            None => {
                unimplemented!();
            }
        }?;

        Ok(children)
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
enum FiberBody<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    Root,
    Text(String, Option<R::TextInstanceKey>),
    Component {
        instance: Box<dyn AnyComponent<P>>,
        state: Box<dyn StoredState>,
        props: Box<dyn StoredProps>,
    },
    Native {
        instance: P,
        native_instance_key: Option<R::InstanceKey>,
        props: P::Props,
    },
}

fn process_wrap<P, R>(
    mut f: impl FnMut(&Fiber<P, R>) -> sorcery::Result<()>,
) -> impl FnMut(&Fiber<P, R>) -> sorcery::Result<Option<ArenaNodeId>>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    move |fiber| {
        f(fiber)?;
        Ok(fiber.child)
    }
}

fn process_wrap_mut<E, P, R>(
    mut f: impl FnMut(&mut Fiber<P, R>) -> Result<(), E>,
) -> impl FnMut(&mut Fiber<P, R>) -> Result<Option<ArenaNodeId>, E>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    E: std::error::Error + 'static,
{
    move |fiber| {
        f(fiber)?;
        Ok(fiber.child)
    }
}

fn walk_fibers<P, R>(
    arena: &Arena<Fiber<P, R>>,
    start: ArenaNodeId,
    mut process: impl FnMut(&Fiber<P, R>, &ArenaNodeId) -> sorcery::Result<Option<ArenaNodeId>>,
) -> sorcery::Result<()>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    let root = start;
    let mut current = start;
    loop {
        let child = if let Some(node) = arena.get(current) {
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
            let current_node = arena.get(current).unwrap(); // todo
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
        let current_node = arena.get(current).unwrap(); // todo

        if let Some(sibling) = current_node.sibling {
            current = sibling;
        }
    }
}

fn walk_fiber_ids<P, R>(
    arena: &mut Arena<Fiber<P, R>>,
    start: ArenaNodeId,
    mut process: impl FnMut(
        &ArenaNodeId,
        &mut Arena<Fiber<P, R>>,
    ) -> sorcery::Result<Option<ArenaNodeId>>,
) -> sorcery::Result<()>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    let root = start;
    let mut current = start;
    loop {
        let child = process(&current, arena)?;
        if let Some(child) = child {
            current = child;
            continue;
        }

        if current == root {
            return Ok(());
        }

        loop {
            let current_node = arena.get(current).unwrap(); // todo
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
        let current_node = arena.get(current).unwrap(); // todo

        if let Some(sibling) = current_node.sibling {
            current = sibling;
        }
    }
}

fn walk_fibers_mut<E, P, R>(
    arena: &mut Arena<Fiber<P, R>>,
    start: ArenaNodeId,
    mut process: impl FnMut(&mut Fiber<P, R>) -> Result<Option<ArenaNodeId>, E>,
) -> Result<(), E>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    E: std::error::Error + 'static,
{
    let root = start;
    let mut current = start;
    loop {
        let child = if let Some(current) = arena.get_mut(current) {
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
            let current_node = arena.get(current).unwrap(); // todo
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
        let current_node = arena.get(current).unwrap(); // todo

        if let Some(sibling) = current_node.sibling {
            current = sibling;
        }
    }
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    pub fn new(renderer: R) -> Self {
        let (events_tx, events_rx) = mpsc::unbounded_channel();
        let (component_events_tx, component_events_rx) = mpsc::unbounded_channel();
        Self {
            renderer: Arc::new(RefCell::new(renderer)),
            next_fiber_id: 0,
            element_type: std::marker::PhantomData,
            events_rx,
            events_tx,
            component_events_rx,
            component_events_tx,
        }
    }

    pub fn create_container(&mut self, base: &mut R::Container) {}

    fn commit(
        &mut self,
        fibers: &mut Arena<Fiber<P, R>>,
        root_id: ArenaNodeId,
        container: &mut R::Container,
    ) -> Result<(), R::Error> {
        let (tx, rx) = channel::unbounded::<Update>();
        walk_fibers_mut(
            fibers,
            root_id,
            process_wrap_mut(|fiber| {
                match &mut fiber.body {
                    Some(FiberBody::Text(txt, ref mut instance_key)) => {
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
                    }) => {
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

        for update in rx.try_iter() {
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
                            container,
                            fibers
                                .get_mut(child)
                                .unwrap()
                                .native_instance_key()
                                .unwrap()
                                .clone(),
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
            }
        }
        Ok(())
    }

    pub async fn run(&mut self) {
        debug!("running");
        let mut current_fibers: Option<Arena<Fiber<P, R>>> = None;
        loop {
            tokio::select! {
                Some(event) = self.events_rx.recv() => {
                    debug!("event: {:?}", event);
                match event {
                    Event::InitialRender {
                        mut fibers,
                        root_id,
                        mut container,
                    } => {
                        self.commit(&mut fibers, root_id, &mut container).expect("todo");
                        current_fibers = Some(fibers);
                    }
                }
                }
                Some(event) = self.component_events_rx.recv() => {
                    match event {
                        ComponentUpdate::SetState {mut context, pointer} => {
                            let context = context.as_any().downcast_mut::<FiberComponentContext>().unwrap();
                            debug!("SET STATE {:?}", context);
                            let fiber = current_fibers.as_ref().unwrap().iter().find(|(_, n)| n.id == context.id).unwrap();
                            debug!("fibber: {:?}", fiber);
                        }
                    }
                }
            };
        }
    }

    pub fn update_container(
        &mut self,
        ctx: &mut Context<P, R>,
        container: R::Container,
        element: &Element<P>,
    ) -> Result<(), R::Error> {
        self.renderer.borrow().schedule_task(
            TaskPriority::Immediate,
            Box::new(RenderTask::<P, R>::new(
                self.events_tx.clone(),
                self.component_events_tx.clone(),
                element.clone(),
                container,
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

fn build_tree<'a, P, R, F>(
    tx: &mpsc::UnboundedSender<ComponentUpdate>,
    arena: &mut Arena<Fiber<P, R>>,
    element: &Element<P>,
    mut next_fiber_id: F,
) -> Result<ArenaNodeId, R::Error>
where
    P: RenderPrimitive,
    R: Renderer<P>,
    F: FnMut() -> FiberId,
    F: Clone,
{
    debug!("rendering a {:?}\n\n", element);
    let children = element.children();
    let fiber = match element {
        Element::Text(txt) => {
            let fiber = Fiber::text(tx.clone(), next_fiber_id(), txt.to_owned());
            Ok(fiber)
        }
        Element::Component(comp_element) => {
            let instance = comp_element.construct().map_err(Error::Sorcery)?;
            let fiber = Fiber::component(
                tx.clone(),
                next_fiber_id(),
                instance,
                comp_element.clone_props(),
            );
            Result::<_, R::Error>::Ok(fiber)
        }
        Element::Native(native) => {
            let instance = native.ty.clone();
            let fiber = Fiber::native(tx.clone(), next_fiber_id(), instance, native.props.clone());
            Ok(fiber)
        }
    }?;
    let id = arena.insert(fiber);
    {
        let to_child = {
            let node = arena.get_mut(id).unwrap();
            node.render(&children)
                .map_err(Error::Sorcery)?
                .into_iter()
                .rev()
                .fold(Result::<_, R::Error>::Ok(None), |prev, next| {
                    let child_id = build_tree(tx, arena, &next, next_fiber_id.clone())?;
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

struct RenderTask<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    root: Element<P>,
    tx: mpsc::UnboundedSender<Event<P, R>>,
    component_tx: mpsc::UnboundedSender<ComponentUpdate>,
    container: R::Container,
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
        container: R::Container,
    ) -> Self {
        Self {
            root,
            tx,
            component_tx,
            container,
        }
    }
}

type TaskResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

impl<P, R> Task for RenderTask<P, R>
where
    P: RenderPrimitive + 'static,
    R: Renderer<P> + 'static,
{
    fn run(self: Box<Self>) -> TaskResult<()> {
        debug!("rendering {:?}", self.root);
        let mut fibers = Arena::<Fiber<P, R>>::new();
        let mut next_fiber_id_ = 0;
        let mut next_fiber_id = move || {
            let id = next_fiber_id_;
            next_fiber_id_ += 1;
            FiberId(id)
        };
        let root_id = {
            let fiber_id = next_fiber_id();
            let node_id = build_tree(&self.component_tx, &mut fibers, &self.root, next_fiber_id)?;
            let mut root_fiber = Fiber {
                context: sorcery::ComponentContext::new(
                    self.component_tx.clone(),
                    FiberComponentContext::new(fiber_id),
                ),
                body: Some(FiberBody::Root),
                id: fiber_id,
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
            .send(Event::InitialRender {
                fibers,
                root_id,
                container: self.container,
            })
            .unwrap();
        Ok(())
    }
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

pub trait Renderer<P>
where
    P: RenderPrimitive,
{
    type Error: std::error::Error;
    type Container;
    type InstanceKey: std::fmt::Debug;
    type TextInstanceKey: std::fmt::Debug;
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

#[cfg(test)]
mod test {
    use test_env_log::test;

    use super::{Context, NativeElement, Reconciler, RenderPrimitive, Renderer};
    use generational_arena::{Arena, Index as ArenaIndex};
    use sorcery::{component, use_state, Component, ComponentContext, Element};

    struct StringRenderer {
        arena: Arena<StringNode>,
    }

    impl StringRenderer {
        fn new() -> Self {
            Self {
                arena: Arena::new(),
            }
        }
    }

    #[derive(Debug, Clone)]
    struct Str(String);

    #[derive(Debug, Clone)]
    struct StringNode {
        value: String,
        children: Vec<ArenaIndex>,
    }

    #[derive(thiserror::Error, Debug)]
    #[error("error {0}")]
    struct Error(String);

    impl StringNode {
        fn to_string(&self, arena: &Arena<StringNode>) -> String {
            format!(
                "{}{}",
                self.value,
                self.children
                    .iter()
                    .filter_map(|c| arena.get(*c).map(|n| n.to_string(arena)))
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
        ) -> sorcery::Result<Vec<Element<Self>>> {
            Ok(children.to_vec())
        }
    }

    // impl Renderer<Str> for StringRenderer {
    //     type Container = StringNode;
    //     type InstanceKey = ArenaIndex;
    //     type TextInstanceKey = ArenaIndex;
    //     type Error = Error;
    //     fn append_text_to_parent(
    //         &mut self,
    //         parent: &Self::InstanceKey,
    //         text: &Self::TextInstanceKey,
    //     ) -> std::result::Result<(), Self::Error> {
    //         Ok(())
    //     }
    //     fn create_instance(&mut self, ty: &Str, props: &()) -> Result<Self::InstanceKey, Error> {
    //         let node = StringNode {
    //             value: ty.0.to_owned(),
    //             children: vec![],
    //         };
    //         let id = self.arena.insert(node);
    //         Ok(id)
    //     }

    //     fn append_child_to_parent<'r>(
    //         &mut self,
    //         parent: &Self::InstanceKey,
    //         child: &Self::InstanceKey,
    //     ) -> Result<(), Error> {
    //         tracing::debug!("append {:?} to {:?}", child, parent);
    //         if let Some(p) = self.arena.get_mut(*parent) {
    //             p.children.push(*child);
    //         }
    //         Ok(())
    //     }

    //     fn append_child_to_container<'r>(
    //         &mut self,
    //         container: &'r mut Self::Container,
    //         child: &Self::InstanceKey,
    //     ) -> Result<(), Error> {
    //         tracing::debug!("append {:?} to {:?}", child, container);
    //         container.children.push(*child);
    //         Ok(())
    //     }
    // }

    // struct List {}

    // impl Component<Str> for List {
    //     type Props = ();

    //     fn name(&self) -> String {
    //         "List".to_string()
    //     }

    //     fn new(props: &Self::Props) -> Self {
    //         Self {}
    //     }

    //     fn render(
    //         &self,
    //         context: &mut ComponentContext,
    //         props: &Self::Props,
    //         children: &[Element<Str>],
    //     ) -> sorcery::Result<Element<Str>> {
    //         let (index, set_index) = use_state(context, 1);
    //         Ok(Element::native(
    //             None,
    //             Str("test".to_string()),
    //             (),
    //             children.to_vec(),
    //         ))
    //     }
    // }
    // #[test]
    // fn it_creates_element() {
    //     let renderer = StringRenderer::new();
    //     let mut reconciler = Reconciler::new(renderer);
    //     let mut container = StringNode {
    //         value: "".to_owned(),
    //         children: vec![],
    //     };
    //     reconciler.create_container(&mut container);
    //     let component = Element::component::<List>(
    //         None,
    //         (),
    //         vec![Element::native(None, Str("hello".to_string()), (), vec![])],
    //     );
    //     reconciler
    //         .update_container(&mut Context::new(), &mut container, &component)
    //         .unwrap();
    //     tracing::trace!("UHHH {:?}", container);

    //     // reconciler
    //     //     .update_container(&mut container, &component)
    //     //     .unwrap();
    //     assert_eq!(
    //         "testhello",
    //         &container.to_string(&reconciler.renderer().arena)
    //     );
    // }
}
