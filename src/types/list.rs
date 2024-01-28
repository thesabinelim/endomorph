macro_rules! ListOf {
    () => {
        $crate::types::list::Nil
    };
    (..$list:ty) => {
        $list
    };
    ($item:ty) => {
        $crate::types::list::ListOf![$item,]
    };
    (..$list:ty, $($rest:tt)*) => {
        <$list as List>::ConcatResult<$crate::types::list::ListOf![$($rest)*]>
    };
    ($item:ty, $($rest:tt)*) => {
        $crate::types::list::Cons<$item, $crate::types::list::ListOf![$($rest)*]>
    };
}

pub(crate) use ListOf;

macro_rules! list {
    () => {
        $crate::types::list::Nil
    };
    (..$list:expr) => {
        $list
    };
    ($item:expr) => {
        $crate::types::list::list![$item,]
    };
    (..$list:expr, $($rest:tt)*) => {
        $list.concat($crate::types::list::list![$($rest)*])
    };
    ($item:expr, $($rest:tt)*) => {
        $crate::types::list::Cons($item, $crate::types::list::list![$($rest)*])
    };
}

pub(crate) use list;

macro_rules! ListPat {
    () => {
        $crate::types::list::Nil
    };
    (..$list:pat) => {
        $list
    };
    ($item:pat) => {
        $crate::types::list::ListPat![$item,]
    };
    ($item:pat, $($rest:tt)*) => {
        $crate::types::list::Cons($item, $crate::types::list::ListPat![$($rest)*])
    };
}

pub(crate) use ListPat;

#[derive(Clone, PartialEq, Debug)]
pub struct Cons<Item, Rest>(pub Item, pub Rest)
where
    Item: Clone,
    Rest: List;

#[derive(Clone, PartialEq, Debug)]
pub struct Nil;

pub trait List: Clone {
    const LEN: usize;

    fn len(&self) -> usize {
        Self::LEN
    }

    type AppendResult<T>: List
    where
        T: Clone;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone;

    type ConcatResult<T>: List
    where
        T: List;

    fn concat<T>(&self, list: T) -> Self::ConcatResult<T>
    where
        T: List;

    type Reversed: List;

    fn reverse(&self) -> Self::Reversed;
}

pub trait NonEmptyList: List {
    type Item;
    type LastItem;
    type Rest: List;

    type WithoutLastItem: List;

    fn pop(self) -> (Self::LastItem, Self::WithoutLastItem);
}

pub trait EmptyList: List {}

impl<Item, Rest> List for Cons<Item, Rest>
where
    Item: Clone,
    Rest: List,
{
    const LEN: usize = Rest::LEN + 1;

    type AppendResult<T> = Cons<Item, Rest::AppendResult<T>>
    where
        T: Clone;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone,
    {
        let Cons(self_item, rest) = self;
        Cons(self_item.clone(), rest.append(item))
    }

    type ConcatResult<T> = Cons<Item, Rest::ConcatResult<T>>
    where
        T: List;

    fn concat<T>(&self, list: T) -> Self::ConcatResult<T>
    where
        T: List,
    {
        let Cons(item, rest) = self;
        Cons(item.clone(), rest.concat(list))
    }

    type Reversed = <Rest::Reversed as List>::AppendResult<Item>;

    fn reverse(&self) -> Self::Reversed {
        let Cons(item, rest) = self;
        rest.reverse().append(item.clone())
    }
}

impl<Item, Rest> NonEmptyList for Cons<Item, Rest>
where
    Item: Clone,
    Rest: NonEmptyList,
{
    type Item = Item;
    type LastItem = Rest::LastItem;
    type Rest = Rest;

    type WithoutLastItem = Cons<Item, Rest::WithoutLastItem>;

    fn pop(self) -> (Self::LastItem, Self::WithoutLastItem) {
        let Cons(item, rest) = self;
        let (last, without_last) = rest.pop();
        (last, Cons(item, without_last))
    }
}

impl<Item> NonEmptyList for Cons<Item, Nil>
where
    Item: Clone,
{
    type Item = Item;
    type LastItem = Item;
    type Rest = Nil;

    type WithoutLastItem = Nil;

    fn pop(self) -> (Self::LastItem, Self::WithoutLastItem) {
        let Cons(item, Nil) = self;
        (item, Nil)
    }
}

impl List for Nil {
    const LEN: usize = 0;

    type AppendResult<T> = Cons<T, Self>
    where
        T: Clone;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone,
    {
        Cons(item, Nil)
    }

    type ConcatResult<T> = T
    where
        T: List;

    fn concat<T>(&self, list: T) -> Self::ConcatResult<T>
    where
        T: List,
    {
        list
    }

    type Reversed = Self;

    fn reverse(&self) -> Self::Reversed {
        self.clone()
    }
}

impl EmptyList for Nil {}
