#![feature(associated_type_defaults)]

use dyn_clone::DynClone;
pub use sorcery_codegen::component;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    fmt::{self, Debug},
};

pub use sorcery_macros::{rsx, Props};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("invalid props")]
    InvalidProps,
    #[error("no such element with name `{0}`")]
    InvalidNativeName(String),
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

pub trait StoredProps: Any + DynClone + std::fmt::Debug {
    fn any(&self) -> &(dyn Any + '_);
}

dyn_clone::clone_trait_object!(StoredProps);

impl<T> StoredProps for T
where
    T: Any + Clone + std::fmt::Debug,
{
    fn any(&self) -> &dyn Any {
        self
    }
}

pub trait StoredState: Any {
    fn any(&self) -> &(dyn Any + '_);
}

impl fmt::Debug for dyn StoredState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StoredState").finish()
    }
}

impl<T> StoredState for T
where
    T: Any,
{
    fn any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct ComponentElement<T, O>
where
    T: RenderPrimitive<O>,
{
    name: Option<String>,
    key: Option<Key>,
    constructor: fn(&dyn StoredProps) -> Result<Box<dyn AnyComponent<T, O>>>,
    props: Box<dyn StoredProps>,
    children: Vec<Element<T, O>>,
}

impl<T, O> Debug for ComponentElement<T, O>
where
    T: RenderPrimitive<O>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ComponentElement")
            .field("name", &self.name.as_deref().unwrap_or_else(|| "?"))
            .finish()
    }
}

impl<T, O> ComponentElement<T, O>
where
    T: RenderPrimitive<O>,
{
    pub fn construct(&self) -> Result<Box<dyn AnyComponent<T, O>>> {
        (self.constructor)(self.props.as_ref())
    }

    pub fn props(&self) -> &dyn StoredProps {
        self.props.as_ref()
    }

    pub fn clone_props(&self) -> Box<dyn StoredProps> {
        self.props.clone()
    }
}

#[derive(Debug, Clone)]
pub struct NativeElement<T, O>
where
    T: RenderPrimitive<O> + std::fmt::Debug,
{
    pub key: Option<Key>,
    pub ty: T,
    pub props: T::Props,
    pub children: Vec<Element<T, O>>,
}

#[derive(Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct ElementId(u32);

#[derive(Debug, Clone)]
pub enum Element<T, O>
where
    T: RenderPrimitive<O>,
{
    Component(ComponentElement<T, O>),
    Native(NativeElement<T, O>),
    Text(String),
}

pub trait Props {
    type Builder;
    fn builder() -> Self::Builder;
}

pub trait RenderPrimitive<O>: std::fmt::Debug + Clone {
    type Props: Props + std::fmt::Debug + Clone;
    fn for_name(name: &str) -> Option<Self>;
    fn render<'r>(
        &self,
        props: &'r Self::Props,
        children: &[Element<Self, O>],
    ) -> Result<Vec<Element<Self, O>>>;
}

impl<T, O> Element<T, O>
where
    T: RenderPrimitive<O>,
{
    pub fn component<C>(key: Option<Key>, props: C::Props, children: Vec<Element<T, O>>) -> Self
    where
        C: Component<T, O> + 'static,
        T: 'static,
    {
        let constructor = |props: &dyn StoredProps| <C as AnyComponent<T, O>>::new(props);
        let name = std::any::type_name::<C>();
        Element::Component(ComponentElement {
            key,
            name: Some(name.to_string()),
            constructor: constructor,
            props: Box::new(props),
            children,
        })
    }

    pub fn props_builder() -> <<T as RenderPrimitive<O>>::Props as Props>::Builder {
        T::Props::builder()
    }

    pub fn native_for_name(
        key: Option<Key>,
        name: &str,
        props: T::Props,
        children: Vec<Element<T, O>>,
    ) -> Result<Self> {
        let ty = T::for_name(name).ok_or_else(|| Error::InvalidNativeName(name.to_owned()))?;
        Ok(Self::native(key, ty, props, children))
    }

    pub fn native(key: Option<Key>, ty: T, props: T::Props, children: Vec<Element<T, O>>) -> Self {
        Element::Native(NativeElement {
            key,
            ty,
            props,
            children,
        })
    }

    pub fn text(text: impl Into<String>) -> Self {
        Element::Text(text.into())
    }

    pub fn children(&self) -> &[Element<T, O>] {
        match self {
            Element::Text(_) => &[],
            Element::Component(comp_element) => &comp_element.children,
            Element::Native(native) => &native.children,
        }
    }
}

pub trait AnyComponent<T, O>
where
    T: RenderPrimitive<O>,
{
    fn name(&self) -> String;
    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T, O>>>
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut O,
        props: &dyn StoredProps,
        children: &[Element<T, O>],
    ) -> Result<Element<T, O>>;
}

impl<P, O> Debug for dyn AnyComponent<P, O>
where
    P: RenderPrimitive<O>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("AnyComponent({})", self.name()))
    }
}

pub trait Component<T, O>
where
    T: RenderPrimitive<O>,
{
    type Props: StoredProps;
    type __Primitive = T;
    fn name(&self) -> String {
        std::any::type_name::<Self>().to_string()
    }
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut O,
        props: &Self::Props,
        children: &[Element<T, O>],
    ) -> Result<Element<T, O>>;
}

impl<C, P, T, O> AnyComponent<T, O> for C
where
    C: Component<T, O, Props = P> + 'static,
    P: 'static,
    T: RenderPrimitive<O>,
{
    fn name(&self) -> String {
        C::name(self)
    }

    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T, O>>>
    where
        Self: Sized,
    {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        Ok(Box::new(C::new(props)) as Box<dyn AnyComponent<T, O>>)
    }

    fn render(
        &self,
        context: &mut O,
        props: &dyn StoredProps,
        children: &[Element<T, O>],
    ) -> Result<Element<T, O>> {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        C::render(self, context, props, children)
    }
}

fn as_any<C, T, O>(component: C) -> Box<dyn AnyComponent<T, O>>
where
    C: Component<T, O> + 'static,
    T: RenderPrimitive<O>,
    O: ComponentContext,
{
    Box::new(component)
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ComponentId(u32);

impl From<u32> for ComponentId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

pub trait StateUpdater<T>: Fn(T) + Send + Sync {}

pub trait ComponentContext {
    fn register_state_hook<T>(&mut self, initial: T) -> (T, Box<dyn StateUpdater<T>>);
}

pub fn use_state<O, T>(context: &mut O, initial: T) -> (T, impl Fn(T) + Send + Sync)
where
    T: Any,
    O: ComponentContext,
{
    context.register_state_hook(initial)
}

pub fn use_effect<O>(context: &mut O, f: impl Fn())
where
    O: ComponentContext,
{
}

enum HookState {
    State(Box<dyn Any>),
    Effect(Vec<Box<dyn Dep>>),
}

pub trait Dep {
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
    use test_env_log::test;
    mod sorcery {
        pub use super::super::*;
    }
    use super::{component, use_effect, use_state, Component, ComponentContext, Key, Result};

    struct TestElement {}

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

    // #[test]
    // fn it_creates_element() {
    //     // let rendered =
    //     //     test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
    //     // assert_eq!(rendered, "* a\n* b\n* c\n");
    // }

    // #[test]
    // fn macro_yields_same_result() {
    //     // let rendered =
    //     //     test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
    //     // let macro_rendered =
    //     //     test_render::<List>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
    //     // assert_eq!(rendered, macro_rendered);
    // }
}
