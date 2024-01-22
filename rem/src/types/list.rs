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

pub trait NonEmptyList: List {
    type Item;
    type Rest: List;
}

#[derive(Clone, PartialEq, Debug)]
pub struct Cons<Item, Rest>(pub Item, pub Rest)
where
    Rest: List;

pub trait EmptyList: List {}

#[derive(Clone, PartialEq, Debug)]
pub struct Nil;

pub trait List {
    const LEN: usize;

    fn len(&self) -> usize {
        Self::LEN
    }

    type AppendResult<T>: List;

    fn append<T>(self, item: T) -> Self::AppendResult<T>;

    type ConcatResult<T>: List
    where
        T: List;

    fn concat<T>(self, list: T) -> Self::ConcatResult<T>
    where
        T: List;

    type ReverseResult: List;

    fn reverse(self) -> Self::ReverseResult;
}

impl<Item, Rest> NonEmptyList for Cons<Item, Rest>
where
    Rest: List,
{
    type Item = Item;
    type Rest = Rest;
}

impl<Item, Rest> List for Cons<Item, Rest>
where
    Rest: List,
{
    const LEN: usize = Rest::LEN + 1;

    type AppendResult<T> = Cons<Item, Rest::AppendResult<T>>;

    fn append<T>(self, item: T) -> Self::AppendResult<T> {
        let Cons(self_item, rest) = self;
        Cons(self_item, rest.append(item))
    }

    type ConcatResult<T> = Cons<Item, Rest::ConcatResult<T>>
    where
        T: List;

    fn concat<T>(self, list: T) -> Self::ConcatResult<T>
    where
        T: List,
    {
        let Cons(item, rest) = self;
        Cons(item, rest.concat(list))
    }

    type ReverseResult = <Rest::ReverseResult as List>::AppendResult<Item>;

    fn reverse(self) -> Self::ReverseResult {
        let Cons(item, rest) = self;
        rest.reverse().append(item)
    }
}

impl EmptyList for Nil {}

impl List for Nil {
    const LEN: usize = 0;

    type AppendResult<T> = Cons<T, Self>;

    fn append<T>(self, item: T) -> Self::AppendResult<T> {
        Cons(item, Nil)
    }

    type ConcatResult<T> = T
    where
        T: List;

    fn concat<T>(self, list: T) -> Self::ConcatResult<T>
    where
        T: List,
    {
        list
    }

    type ReverseResult = Nil;

    fn reverse(self) -> Self::ReverseResult {
        self
    }
}
