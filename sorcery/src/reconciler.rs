use crate::{
    Component, ComponentElement, ComponentId, Element, Key, NativeElement, RenderPrimitive, Result,
};
use std::{
    any::Any,
    collections::{HashMap, VecDeque},
};

pub struct Reconciler<P, R>
where
    R: Renderer<P>,
    P: RenderPrimitive,
{
    instances: HashMap<ComponentId, Box<dyn Any>>,
    element_type: std::marker::PhantomData<P>,
    renderer: R,
    render_queue: VecDeque<Element<P>>,
}

#[derive(Debug)]
enum RenderNodeElement<'r, T>
where
    T: RenderPrimitive,
{
    Native { element: &'r NativeElement<T> },
    Component { element: &'r ComponentElement<T> },
}

#[derive(Debug)]
struct RenderNode<'r, T>
where
    T: RenderPrimitive,
{
    element: RenderNodeElement<'r, T>,
    dependencies: Vec<RenderNode<'r, T>>,
}

impl<P, R> Reconciler<P, R>
where
    P: RenderPrimitive,
    R: Renderer<P>,
{
    pub fn new(renderer: R) -> Self {
        Self {
            renderer,
            render_queue: VecDeque::new(),
            instances: HashMap::new(),
            element_type: std::marker::PhantomData,
        }
    }

    pub fn create_container(&mut self, base: R::Container) -> R::Container {
        base
    }

    fn build_tree<'r>(&mut self, element: &'r Element<P>) -> Result<RenderNode<'r, P>> {
        println!("rendering a {:?}", element);
        match element {
            Element::Component(comp) => {
                // let c = element.construct()?;
                // let mut context = super::Context::new();
                // let mut ccontext = context.component();
                // let new_element = c.render(&mut ccontext, element.props(), vec![])?;
                let children = comp
                    .children
                    .iter()
                    .map(|child| self.build_tree(child))
                    .collect::<Result<Vec<_>>>()?;
                // let rendered = self.render_tree(&new_element)?;
                Ok(RenderNode {
                    element: RenderNodeElement::Component { element: comp },
                    dependencies: children,
                })
            }
            Element::Native(native) => {
                let children = native
                    .children
                    .iter()
                    .map(|child| self.build_tree(child))
                    .collect::<Result<Vec<_>>>()?;
                Ok(RenderNode {
                    element: RenderNodeElement::Native { element: native },
                    dependencies: children,
                })
            }
        }
    }

    // fn render(&mut self) -> Result<()> {
    //     for element in &self.render_queue {
    //         match element {
    //             Element::Component(comp) => {
    //                 let c = comp.construct()?;
    //                 let mut context = super::Context::new();
    //                 let mut ccontext = context.component();
    //                 let new_element = c.render(&mut ccontext, comp.props(), vec![])?;
    //                 self.schedule_render(&new_element);
    //             }
    //             Element::Native(native) => {}
    //         }
    //     }
    //     Ok(())
    // }

    fn diff(&self, old_tree: &R::Instance, new_tree: &R::Instance) {}

    pub fn update_container<'r>(
        &mut self,
        container: &mut R::Container,
        element: &Element<P>,
    ) -> Result<()> {
        let tree = self.build_tree(element)?;
        println!("nodes {:?}", tree);
        // self.renderer.append_child_to_container(container, tree);
        Ok(())
    }
}

pub trait Renderer<P>
where
    P: RenderPrimitive,
{
    type Container;
    type Instance: Clone + std::fmt::Debug;
    fn create_instance(&mut self, element: &NativeElement<P>) -> Self::Instance;
    fn append_child_to_container(&mut self, container: &mut Self::Container, child: Self::Instance);
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

    #[derive(Debug, Clone)]
    struct Str(String);

    #[derive(Debug, Clone)]
    struct StringNode {
        value: String,
        children: Vec<StringNode>,
    }

    impl StringNode {
        fn to_string(&self) -> String {
            format!(
                "{}{}",
                self.value,
                self.children
                    .iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<_>>()
                    .join("")
            )
        }
    }

    impl RenderPrimitive for Str {
        type Props = ();
    }

    impl Renderer<Str> for StringRenderer {
        type Container = StringNode;
        type Instance = StringNode;
        fn create_instance(&mut self, element: &NativeElement<Str>) -> Self::Instance {
            StringNode {
                value: element.ty.0.to_owned(),
                children: vec![],
            }
        }

        fn append_child_to_container(
            &mut self,
            container: &mut Self::Container,
            child: Self::Instance,
        ) {
            container.children.push(child);
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
            Ok(Element::native(
                None,
                Str("test".to_string()),
                (),
                vec![Element::native(None, Str("hi".into()), (), vec![])],
            ))
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
        let mut container = reconciler.create_container(StringNode {
            value: "".to_owned(),
            children: vec![],
        });
        let component = Element::component::<List>(
            None,
            (),
            vec![Element::native(None, Str("hello".to_string()), (), vec![])],
        );
        reconciler
            .update_container(&mut container, &component)
            .unwrap();
        reconciler
            .update_container(&mut container, &component)
            .unwrap();
        assert_eq!("test", &container.to_string());
    }
}
