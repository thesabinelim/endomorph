macro_rules! list {
    () => {
        $crate::types::list::Nil
    };
    ($item:expr) => { $crate::types::list::list![$item,] };
    ($item:expr, $($rest:tt)*) => {
        $crate::types::list::Cons::new($item, $crate::types::list::list![$($rest)*])
    };
}

pub(crate) use list;

#[derive(Clone, Debug)]
pub struct Cons<Item, Rest>
where
    Rest: List,
{
    item: Item,
    rest: Rest,
}

impl<Item, Rest> Cons<Item, Rest>
where
    Rest: List,
{
    pub fn new(item: Item, rest: Rest) -> Cons<Item, Rest> {
        Cons { item, rest }
    }
}

#[derive(Clone, Debug)]
pub struct Nil;

pub trait List
where
    Self: Clone + PartialEq,
{
    const LEN: usize;
}

impl<Item, Rest> Cons<Item, Rest>
where
    Item: Clone + PartialEq,
    Rest: List,
{
    fn head(&self) -> Item {
        self.item.clone()
    }

    fn rest(&self) -> Rest {
        self.rest.clone()
    }
}

impl<Item, Rest> List for Cons<Item, Rest>
where
    Item: Clone + PartialEq,
    Rest: List,
{
    const LEN: usize = Rest::LEN + 1;
}

impl<Item, Rest, Rhs> PartialEq<Rhs> for Cons<Item, Rest>
where
    Self: List,
    Rest: List,
    Rhs: List,
{
    fn eq(&self, _other: &Rhs) -> bool {
        Self::LEN == Rhs::LEN
    }
}

impl Nil {}

impl List for Nil {
    const LEN: usize = 0;
}

impl<Rhs> PartialEq<Rhs> for Nil
where
    Rhs: List,
{
    fn eq(&self, _other: &Rhs) -> bool {
        Self::LEN == Rhs::LEN
    }
}
