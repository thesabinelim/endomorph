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

pub trait ListCons: List {
    type Item: Clone + PartialEq;
    type Rest: List;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Cons<Item, Rest>(pub Item, pub Rest)
where
    Item: Clone + PartialEq,
    Rest: List;

pub trait ListNil: List {}

#[derive(Clone, PartialEq, Debug)]
pub struct Nil;

pub trait List: Clone + PartialEq {
    const LEN: usize;

    fn len(&self) -> usize {
        Self::LEN
    }

    type AppendResult<T>: List
    where
        T: Clone + PartialEq;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone + PartialEq;

    type ConcatResult<T>: List
    where
        T: List;

    fn concat<T>(&self, list: T) -> Self::ConcatResult<T>
    where
        T: List;
}

impl<Item, Rest> ListCons for Cons<Item, Rest>
where
    Item: Clone + PartialEq,
    Rest: List,
{
    type Item = Item;
    type Rest = Rest;
}

impl<Item, Rest> List for Cons<Item, Rest>
where
    Item: Clone + PartialEq,
    Rest: List,
{
    const LEN: usize = Rest::LEN + 1;

    type AppendResult<T> = Cons<Item, Rest::AppendResult<T>>
    where
        T: Clone + PartialEq;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone + PartialEq,
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
}

impl ListNil for Nil {}

impl List for Nil {
    const LEN: usize = 0;

    type AppendResult<T> = Cons<T, Self>
    where
        T: Clone + PartialEq;

    fn append<T>(&self, item: T) -> Self::AppendResult<T>
    where
        T: Clone + PartialEq,
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
}
