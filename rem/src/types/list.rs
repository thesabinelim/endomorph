macro_rules! ListOf {
    () => {
        $crate::types::list::Nil
    };
    (..$rest:ty) => {
        $rest
    };
    ($item:ty) => {
        $crate::types::list::ListOf![$item,]
    };
    ($item:ty, $($items:tt)*) => {
        $crate::types::list::Cons<$item, $crate::types::list::ListOf![$($items)*]>
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
    (..$rest:pat) => {
        $rest
    };
    ($item:pat) => {
        $crate::types::list::ListPat![$item,]
    };
    ($item:pat, $($items:tt)*) => {
        $crate::types::list::Cons($item, $crate::types::list::ListPat![$($items)*])
    };
}

pub(crate) use ListPat;

pub trait ListCons: List {}

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
