mod reconciler;
pub use reconciler::Renderer;
pub use sorcery_codegen::component;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{self, Debug},
};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("invalid props")]
    InvalidProps,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Key(String);

impl<I> From<I> for Key
where
    I: Into<String>,
{
    fn from(key: I) -> Self {
        Key(key.into())
    }
}

pub trait StoredProps: Any + std::fmt::Debug {
    fn any(&self) -> &(dyn Any + '_);
}

impl<T> StoredProps for T
where
    T: Any + Clone + std::fmt::Debug,
{
    fn any(&self) -> &dyn Any {
        self
    }
}

pub struct ComponentElement<T>
where
    T: RenderPrimitive,
{
    name: Option<String>,
    key: Option<Key>,
    constructor: fn(&dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>,
    props: Box<dyn StoredProps>,
    children: Vec<Element<T>>,
}

impl<T> Debug for ComponentElement<T>
where
    T: RenderPrimitive,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ComponentElement")
            .field("name", &self.name.as_deref().unwrap_or_else(|| "?"))
            .finish()
    }
}

impl<T> ComponentElement<T>
where
    T: RenderPrimitive,
{
    pub(crate) fn construct(&self) -> Result<Box<dyn AnyComponent<T>>> {
        (self.constructor)(self.props.as_ref())
    }

    pub(crate) fn props(&self) -> &dyn StoredProps {
        self.props.as_ref()
    }
}

#[derive(Debug)]
pub struct NativeElement<T>
where
    T: RenderPrimitive + std::fmt::Debug,
{
    key: Option<Key>,
    ty: T,
    props: T::Props,
    children: Vec<Element<T>>,
}

#[derive(Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct ElementId(u32);

#[derive(Debug)]
pub enum Element<T>
where
    T: RenderPrimitive,
{
    Component(ComponentElement<T>),
    Native(NativeElement<T>),
}

pub trait RenderPrimitive: std::fmt::Debug {
    type Props: std::fmt::Debug + Default;
}

impl<T> Element<T>
where
    T: RenderPrimitive,
{
    pub fn component<C>(key: Option<Key>, props: C::Props, children: Vec<Element<T>>) -> Self
    where
        C: Component<T> + 'static,
        T: 'static,
    {
        let constructor = |props: &dyn StoredProps| <C as AnyComponent<T>>::new(props);
        let name = std::any::type_name::<C>();
        Element::Component(ComponentElement {
            key,
            name: Some(name.to_string()),
            constructor: constructor,
            props: Box::new(props),
            children,
        })
    }

    pub fn native(key: Option<Key>, ty: T, props: T::Props, children: Vec<Element<T>>) -> Self {
        Element::Native(NativeElement {
            key,
            ty,
            props,
            children,
        })
    }
}

impl<T> From<T> for Element<T>
where
    T: RenderPrimitive,
{
    fn from(element: T) -> Self {
        Element::native(None, element, T::Props::default(), vec![])
    }
}

impl<K, T> From<(K, T)> for Element<T>
where
    K: Into<Key>,
    T: RenderPrimitive,
{
    fn from((key, element): (K, T)) -> Self {
        Element::native(Some(key.into()), element, T::Props::default(), vec![])
    }
}

trait AnyComponent<T>
where
    T: RenderPrimitive,
{
    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut ComponentContext,
        props: &dyn StoredProps,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>>;
}

pub trait Component<T>
where
    T: RenderPrimitive,
{
    type Props: StoredProps;
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut ComponentContext,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>>;
}

impl<C, P, T> AnyComponent<T> for C
where
    C: Component<T, Props = P> + 'static,
    P: 'static,
    T: RenderPrimitive,
{
    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>
    where
        Self: Sized,
    {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        Ok(Box::new(C::new(props)) as Box<dyn AnyComponent<T>>)
    }

    fn render(
        &self,
        context: &mut ComponentContext,
        props: &dyn StoredProps,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>> {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        C::render(self, context, props, children)
    }
}

fn as_any<C, T>(component: C) -> Box<dyn AnyComponent<T>>
where
    C: Component<T> + 'static,
    T: RenderPrimitive,
{
    Box::new(component)
}

#[derive(Debug)]
pub enum Op {}

pub fn reconcile<T>(old_tree: &Element<T>, new_tree: &Element<T>) -> Vec<Op>
where
    T: RenderPrimitive,
{
    // match (old_tree, new_tree) {
    //     (Element::None, Element::None) => vec![],
    //     (Element::None, Element::)
    // }
    vec![]
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ComponentId(u32);

impl From<u32> for ComponentId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

pub struct Context {
    next_id: u32,
    state: HashMap<ComponentId, Vec<HookState>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            state: HashMap::new(),
        }
    }

    pub fn next_id(&mut self) -> ComponentId {
        let id = self.next_id;
        self.next_id += 1;
        id.into()
    }

    pub fn component<'r>(&'r mut self) -> ComponentContext<'r> {
        ComponentContext { context: self }
    }

    pub fn prepare_render(&mut self) {}

    fn register_state_hook(&mut self) {
        // self.state.push(HookState::State())
    }
}

pub struct ComponentContext<'r> {
    context: &'r mut Context,
}

impl<'r> ComponentContext<'r> {
    pub fn register_state_hook(&mut self) {
        self.context.register_state_hook();
    }
}

pub fn use_state<T>(
    context: &mut ComponentContext,
    initial: T,
) -> (T, impl Fn(T) + Send + Sync + Clone)
where
    T: Any,
{
    let value = initial;
    let setter = |v: T| {};
    (value, setter)
}

pub fn use_effect(context: &mut ComponentContext, f: impl Fn()) {}

enum HookState {
    State(Box<dyn Any>),
    Effect(Vec<Box<dyn Dep>>),
}

trait Dep {
    fn as_any(&self) -> &dyn Any;
    fn compare(&self, other: &dyn Dep) -> bool;
}

impl<S: 'static + PartialEq> Dep for S {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn compare(&self, other: &dyn Dep) -> bool {
        other
            .as_any()
            .downcast_ref::<S>()
            .map_or(false, |a| self == a)
    }
}

#[cfg(test)]
mod test {
    mod sorcery {
        pub use super::super::*;
    }
    use super::{
        component, use_effect, use_state, Component, ComponentContext, Context, Key, Result,
    };

    struct TestElement {}
    type Element<'r> = crate::Element<TestElement>;

    // struct List2 {}

    // impl Component<TestElement> for List2 {
    //     type Props = Vec<String>;
    //     fn new(props: &Self::Props) -> Self {
    //         Self {}
    //     }

    //     fn render(
    //         &self,
    //         context: &mut ComponentContext,
    //         props: &Self::Props,
    //         _: Vec<Element>,
    //     ) -> Result<Element> {
    //         let elements = props
    //             .iter()
    //             .map(|s| list_item(context, s, vec![]))
    //             .collect::<Result<_>>()?;
    //         Ok(Element::List(elements))
    //     }
    // }

    // #[component]
    // fn list<E>(props: &Vec<String>, _: Vec<E>) -> Result<E> {
    //     let elements = props
    //         .iter()
    //         .map(|s| list_item(s, vec![]))
    //         .collect::<Result<_>>()?;
    //     Ok(Element::List(elements))
    // }

    #[test]
    fn it_creates_element() {
        // let rendered =
        //     test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        // assert_eq!(rendered, "* a\n* b\n* c\n");
    }

    #[test]
    fn macro_yields_same_result() {
        // let rendered =
        //     test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        // let macro_rendered =
        //     test_render::<List>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        // assert_eq!(rendered, macro_rendered);
    }
}
