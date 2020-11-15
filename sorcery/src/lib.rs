pub use sorcery_codegen::component;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(thiserror::Error, Debug)]
pub enum Error {}

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

#[derive(Debug)]
pub enum Element<T> {
    List(Vec<Element<T>>),
    Component(Option<Key>, TypeId, Box<Element<T>>),
    Native(Option<Key>, T),
    None,
}

impl<T> From<T> for Element<T> {
    fn from(element: T) -> Self {
        Element::Native(None, element)
    }
}

impl<K, T> From<(K, T)> for Element<T>
where
    K: Into<Key>,
{
    fn from((key, element): (K, T)) -> Self {
        Element::Native(Some(key.into()), element)
    }
}

trait ElementCreator<T> {
    type Props;
    fn create_element(
        context: &mut ComponentContext,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>>;

    fn create_element_with_key(
        context: &mut ComponentContext,
        key: impl Into<Key>,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>>;
}

impl<C, T> ElementCreator<T> for C
where
    C: Component<T> + 'static,
{
    type Props = C::Props;

    fn create_element(
        context: &mut ComponentContext,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>> {
        let component = C::new(props);
        Ok(Element::Component(
            None,
            component.type_id(),
            Box::new(component.render(context, props, children)?),
        ))
    }

    fn create_element_with_key(
        context: &mut ComponentContext,
        key: impl Into<Key>,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>> {
        let component = C::new(props);
        Ok(Element::Component(
            Some(key.into()),
            component.type_id(),
            Box::new(component.render(context, props, children)?),
        ))
    }
}

pub trait Component<T> {
    type Props;
    fn new(id: ComponentId, props: &Self::Props) -> Self;
    fn render(
        &self,
        context: &mut ComponentContext,
        props: &Self::Props,
        children: Vec<Element<T>>,
    ) -> Result<Element<T>>;
}

#[derive(Debug)]
pub enum Op {}

pub fn reconcile<T>(old_tree: &Element<T>, new_tree: &Element<T>) -> Vec<Op> {
    // match (old_tree, new_tree) {
    //     (Element::None, Element::None) => vec![],
    //     (Element::None, Element::)
    // }
    vec![]
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ComponentId(u32);

pub struct Context {
    state: HashMap<ComponentId, Vec<HookState>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
        }
    }

    pub fn component<'r>(&'r mut self) -> ComponentContext<'r> {
        ComponentContext { context: self }
    }

    pub fn prepare_render(&mut self) {}

    fn register_state_hook(&mut self) {
        self.state.push(HookState::State())
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
        component, use_effect, use_state, Component, ComponentContext, Context, ElementCreator,
        Key, Result,
    };
    use runtime::{render as test_render, Element, TestElement};

    mod runtime {
        use crate::{reconcile, Component, Context, ElementCreator};

        pub type Element = crate::Element<TestElement>;

        #[derive(Debug)]
        pub enum TestElement {
            String(String),
        }

        pub fn render<C>(props: &C::Props) -> crate::Result<String>
        where
            C: Component<TestElement> + 'static,
        {
            fn render_element(element: &Element) -> String {
                match element {
                    Element::Component(_, _, element) => render_element(&element),
                    Element::List(elements) => elements
                        .into_iter()
                        .map(render_element)
                        .collect::<Vec<_>>()
                        .join(""),
                    Element::Native(_, TestElement::String(string)) => string.to_owned(),
                    Element::None => "".to_string(),
                }
            }
            let mut context = Context::new();
            let tree1 = {
                context.prepare_render();
                let mut component_context = context.component();
                C::create_element(&mut component_context, props, vec![])?
            };
            let tree2 = {
                context.prepare_render();
                let mut component_context = context.component();
                C::create_element(&mut component_context, props, vec![])?
            };
            let ops = reconcile(&tree1, &tree2);
            Ok(render_element(&tree1))
        }
    }

    struct List2 {}

    impl Component<TestElement> for List2 {
        type Props = Vec<String>;
        fn new(props: &Self::Props) -> Self {
            Self {}
        }

        fn render(
            &self,
            context: &mut ComponentContext,
            props: &Self::Props,
            _: Vec<Element>,
        ) -> Result<Element> {
            let elements = props
                .iter()
                .map(|s| list_item(context, s, vec![]))
                .collect::<Result<_>>()?;
            Ok(Element::List(elements))
        }
    }

    #[component(TestElement)]
    fn list(
        context: &mut ComponentContext,
        props: &Vec<String>,
        _: Vec<Element>,
    ) -> Result<Element> {
        let elements = props
            .iter()
            .map(|s| list_item(context, s, vec![]))
            .collect::<Result<_>>()?;
        Ok(Element::List(elements))
    }

    // #[component]
    // fn list<E>(props: &Vec<String>, _: Vec<E>) -> Result<E> {
    //     let elements = props
    //         .iter()
    //         .map(|s| list_item(s, vec![]))
    //         .collect::<Result<_>>()?;
    //     Ok(Element::List(elements))
    // }

    #[component(TestElement)]
    fn list_item(
        context: &mut ComponentContext,
        props: &String,
        _: Vec<Element>,
    ) -> Result<Element> {
        let (i, set_i) = use_state(context, 0);
        use_effect(context, || {
            set_i(1);
        });
        Ok(TestElement::String(format!("* {} {}\n", props, i)))
    }

    #[test]
    fn it_creates_element() {
        let rendered =
            test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        assert_eq!(rendered, "* a\n* b\n* c\n");
    }

    #[test]
    fn macro_yields_same_result() {
        let rendered =
            test_render::<List2>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        let macro_rendered =
            test_render::<List>(&vec!["a".to_string(), "b".to_string(), "c".to_string()]).unwrap();
        assert_eq!(rendered, macro_rendered);
    }
}
