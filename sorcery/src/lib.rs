use dyn_clone::DynClone;
use std::{
    any::{Any, TypeId},
    fmt::{self, Debug},
};
use tokio::sync::mpsc;
use tracing::{debug, warn};

pub use sorcery_macros::{component, rsx, Props};

pub mod reconciler;

pub use reconciler::RenderContext;

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

impl PartialEq<dyn StoredProps> for dyn StoredProps {
    fn eq(&self, _other: &dyn StoredProps) -> bool {
        warn!("todo, props are always equal right now");
        true
    }
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

pub trait StoredState: Any + DynClone {
    fn as_any(&self) -> &(dyn Any + '_);
}

dyn_clone::clone_trait_object!(StoredState);

impl fmt::Debug for dyn StoredState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StoredState").finish()
    }
}

impl<T> StoredState for T
where
    T: Any + Clone,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct ComponentElement<T>
where
    T: RenderPrimitive,
{
    kind_id: TypeId,
    name: Option<String>,
    key: Option<Key>,
    constructor: fn(&dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>,
    props: Box<dyn StoredProps>,
    children: Vec<Element<T>>,
}

impl<T> PartialEq<ComponentElement<T>> for ComponentElement<T>
where
    T: RenderPrimitive,
{
    fn eq(&self, other: &ComponentElement<T>) -> bool {
        self.name == other.name
            && self.key == other.key
            && &self.props == &other.props
            && self.children == other.children
    }
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
    pub fn construct(&self) -> Result<Box<dyn AnyComponent<T>>> {
        (self.constructor)(self.props.as_ref())
    }

    pub fn props(&self) -> &dyn StoredProps {
        self.props.as_ref()
    }

    pub fn clone_props(&self) -> Box<dyn StoredProps> {
        self.props.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeElement<T>
where
    T: RenderPrimitive + std::fmt::Debug,
{
    pub key: Option<Key>,
    pub ty: T,
    pub props: T::Props,
    pub children: Vec<Element<T>>,
}

#[derive(Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub(crate) struct ElementId(u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Element<T>
where
    T: RenderPrimitive,
{
    Component(ComponentElement<T>),
    Native(NativeElement<T>),
    Text(String),
    List(Vec<Element<T>>),
}

pub trait Props {
    type Builder;
    fn builder() -> Self::Builder;
}

pub trait RenderPrimitive: std::fmt::Debug + Clone + PartialEq {
    type Props: Props + std::fmt::Debug + Clone + PartialEq;
    fn for_name(name: &str) -> Option<Self>;
    fn render<'r>(
        &self,
        props: &'r Self::Props,
        children: &[Element<Self>],
    ) -> Result<Vec<Element<Self>>>;
}

impl<T> Element<T>
where
    T: RenderPrimitive,
{
    pub fn component<C>(key: Option<Key>, props: C::Props, children: Vec<Element<T>>) -> Self
    where
        C: Component<T> + Clone + 'static,
        T: 'static,
    {
        let constructor = |props: &dyn StoredProps| <C as AnyComponent<T>>::new(props);
        let name = std::any::type_name::<C>();
        Element::Component(ComponentElement {
            kind_id: TypeId::of::<C>(),
            key,
            name: Some(name.to_string()),
            constructor: constructor,
            props: Box::new(props),
            children,
        })
    }

    pub fn props_builder() -> <<T as RenderPrimitive>::Props as Props>::Builder {
        T::Props::builder()
    }

    pub fn native_for_name(
        key: Option<Key>,
        name: &str,
        props: T::Props,
        children: Vec<Element<T>>,
    ) -> Result<Self> {
        let ty = T::for_name(name).ok_or_else(|| Error::InvalidNativeName(name.to_owned()))?;
        Ok(Self::native(key, ty, props, children))
    }

    pub fn native(key: Option<Key>, ty: T, props: T::Props, children: Vec<Element<T>>) -> Self {
        Element::Native(NativeElement {
            key,
            ty,
            props,
            children,
        })
    }

    pub fn list<I>(elements: I) -> Self
    where
        I: Into<Vec<Element<T>>>,
    {
        Element::List(elements.into())
    }

    pub fn text(text: impl Into<String>) -> Self {
        Element::Text(text.into())
    }

    pub fn children(&self) -> &[Element<T>] {
        match self {
            Element::Text(_) => &[],
            Element::Component(comp_element) => &comp_element.children,
            Element::Native(native) => &native.children,
            Element::List(children) => children,
        }
    }
}

pub trait Dep {
    fn as_any(&self) -> &dyn Any;
    fn compare(&self, other: &dyn Dep) -> bool;
}

impl fmt::Debug for dyn Dep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Dep").finish()
    }
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

pub trait AnyComponent<T>: DynClone
where
    T: RenderPrimitive,
{
    fn kind_id(&self) -> TypeId;
    fn name(&self) -> String;
    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut RenderContext,
        props: &dyn StoredProps,
        children: &[Element<T>],
    ) -> Result<Element<T>>;
}

dyn_clone::clone_trait_object!(<T> AnyComponent<T>);

impl<T> PartialEq<dyn AnyComponent<T>> for dyn AnyComponent<T> {
    fn eq(&self, _other: &dyn AnyComponent<T>) -> bool {
        warn!("todo, components are always equal right now");
        true
    }
}

impl<P> Debug for dyn AnyComponent<P>
where
    P: RenderPrimitive,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("AnyComponent({})", self.name()))
    }
}

pub trait Component<T>: Clone
where
    T: RenderPrimitive,
{
    type Props: StoredProps + Clone;
    fn name(&self) -> String {
        std::any::type_name::<Self>().to_string()
    }
    fn new(props: &Self::Props) -> Self
    where
        Self: Sized;
    fn render(
        &self,
        context: &mut RenderContext,
        props: &Self::Props,
        children: &[Element<T>],
    ) -> Result<Element<T>>;
}

impl<C, P, T> AnyComponent<T> for C
where
    C: Component<T, Props = P> + Clone + 'static,
    P: Clone + Debug + 'static,
    T: RenderPrimitive,
{
    fn kind_id(&self) -> TypeId {
        TypeId::of::<C>()
    }

    fn name(&self) -> String {
        C::name(self)
    }

    fn new(props: &dyn StoredProps) -> Result<Box<dyn AnyComponent<T>>>
    where
        Self: Sized,
    {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        Ok(Box::new(C::new(props)) as Box<dyn AnyComponent<T>>)
    }

    fn render(
        &self,
        context: &mut RenderContext,
        props: &dyn StoredProps,
        children: &[Element<T>],
    ) -> Result<Element<T>> {
        let props = props.any().downcast_ref::<P>().ok_or(Error::InvalidProps)?;
        C::render(self, context, props, children)
    }
}
