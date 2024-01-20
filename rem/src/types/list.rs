pub trait List {
    const LEN: usize;
}

pub trait ListNode: List
where
    Self::Rest: List,
{
    type Item;
    type Rest;
}

pub trait ListEnd: List {}

pub struct Cons<Item, Rest>
where
    Rest: List,
{
    pub item: Item,
    pub rest: Rest,
}

pub struct Nil;

impl<Item, Rest> List for Cons<Item, Rest>
where
    Rest: List,
{
    const LEN: usize = Rest::LEN + 1;
}

impl<Item, Rest> ListNode for Cons<Item, Rest>
where
    Rest: List,
{
    type Item = Item;
    type Rest = Rest;
}

impl List for Nil {
    const LEN: usize = 0;
}

impl ListEnd for Nil {}
