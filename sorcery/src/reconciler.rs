use crate::{Component, ComponentElement, ComponentId, Element, NativeElement, RenderPrimitive};
use std::{any::Any, collections::HashMap};

pub struct Reconciler<P, R>
where
    R: Renderer<P>,
    P: RenderPrimitive,
{
    instances: HashMap<ComponentId, Box<dyn Any>>,
    element_type: std::marker::PhantomData<P>,
    renderer: R,
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    pub fn new(renderer: R) -> Self {
        Self {
            renderer,
            instances: HashMap::new(),
            element_type: std::marker::PhantomData,
        }
    }

    pub fn create_container(&mut self, base: R::Container) -> R::Container {
        base
    }

    fn render_tree(&mut self, element: Element<P>) -> R::Instance {
        match element {
            Element::Component(ComponentElement {
                constructor, props, ..
            }) => {
                // let props = dyn_clone::clone_box(&**props);
                eprintln!("wtf {:?}", props);
                let c = constructor(props.as_ref());
                let mut context = super::Context::new();
                let mut ccontext = context.component();
                c.render(&mut ccontext, props.as_ref(), vec![]);
                unimplemented!();
            }
            Element::Native(NativeElement { .. }) => {
                unimplemented!();
            }
        }
    }

    pub fn update_container<'r>(&mut self, container: &mut R::Container, element: Element<P>) {
        let tree = self.render_tree(element);
        // let mut instance = self.renderer.create_instance(element);
        // self.renderer
        //     .append_child_to_container(container, &mut instance);
    }
}

pub trait Renderer<P>
where
    P: RenderPrimitive,
{
    type Container;
    type Instance;
    fn create_instance(&mut self, element: &NativeElement<P>) -> Self::Instance;
    fn append_child_to_container(
        &mut self,
        container: &mut Self::Container,
        child: &mut Self::Instance,
    );
}

#[cfg(test)]
mod test {
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

    #[derive(Debug)]
    struct Str(String);

    impl RenderPrimitive for Str {
        type Props = ();
    }

    impl Renderer<Str> for StringRenderer {
        type Container = Str;
        type Instance = String;
        fn create_instance(&mut self, element: &NativeElement<Str>) -> Self::Instance {
            "todo".to_string()
        }

        fn append_child_to_container(
            &mut self,
            container: &mut Self::Container,
            child: &mut Self::Instance,
        ) {
            container.0 = child.clone();
        }
    }

    struct List {}

    impl Component<Str> for List {
        type Props = ();
        fn new(props: &Self::Props) -> Self {
            Self {}
        }

        fn render(
            &self,
            context: &mut ComponentContext,
            props: &Self::Props,
            children: Vec<Element<Str>>,
        ) -> Result<Element<Str>> {
            let (index, set_index) = use_state(context, 1);
            Ok(Element::native(None, Str("test".to_string()), (), vec![]))
        }
    }

    // #[component(Str)]
    // fn list(
    //     context: &mut ComponentContext,
    //     props: &Vec<String>,
    //     _: Vec<Element>,
    // ) -> Result<Vec<Element>> {
    //     let elements = props
    //         .iter()
    //         .map(|s| list_item(context, s, vec![]))
    //         .collect::<Result<_>>()?;
    //     Ok(elements)
    // }

    // #[component(Str)]
    // fn list_item(
    //     context: &mut ComponentContext,
    //     props: &String,
    //     _: Vec<Element>,
    // ) -> Result<Element> {
    //     let (i, set_i) = use_state(context, 0);
    //     use_effect(context, || {
    //         set_i(1);
    //     });
    //     Ok(TestElement::String(format!("* {} {}\n", props, i)))
    // }

    #[test]
    fn it_creates_element() {
        let renderer = StringRenderer::new();
        let mut reconciler = Reconciler::new(renderer);
        let mut container = reconciler.create_container(Str("".to_string()));
        let component = Element::component::<List>(None, (), vec![]);
        reconciler.update_container(&mut container, component);
        assert_eq!("test", &container.0);
    }
}
