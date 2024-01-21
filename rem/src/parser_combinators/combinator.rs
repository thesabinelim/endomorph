use core::marker::PhantomData;

use super::{ParseError, Parser, ParserList, TokenStream};
use crate::types::list::{list, List, ListOf, ListPat};

#[derive(Clone, PartialEq)]
pub struct Choice<Input, Parsers>(Parsers, PhantomData<Input>)
where
    Input: TokenStream,
    Parsers: ParserList<Input>;

impl<Input, Parsers> Choice<Input, Parsers>
where
    Input: TokenStream,
    Parsers: ParserList<Input>,
{
    pub fn of(parsers: Parsers) -> Choice<Input, Parsers> {
        Choice(parsers, PhantomData)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ChoiceError {
    AllFailed,
    Unrecoverable,
}

impl<Input, Item, NextItem, Rest> Parser<Input> for Choice<Input, ListOf![Item, NextItem, ..Rest]>
where
    Input: TokenStream,
    Item: Parser<Input>,
    NextItem: Parser<Input>,
    NextItem::Output: List,
    Rest: ParserList<Input>,
    Choice<Input, ListOf![NextItem, ..Rest]>: Parser<Input>,
{
    type Output = ListOf![Item::Output, ..NextItem::Output];
    type Error = ChoiceError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let of = self.0;
        let ListPat![parser, ..rest] = of;
        match parser.parse(input.clone()) {
            Ok(result) => Ok(list![result]),
            Err(ParseError {
                expected,
                recoverable,
                inner_error: _,
            }) => {
                if recoverable {
                    match Choice::of(rest.clone()).parse(input) {
                        Ok(result) => Ok(result),
                        Err(error) => Err(ParseError {
                            expected: expected,
                            recoverable: true,
                            inner_error: ChoiceError::AllFailed,
                        }),
                    }
                } else {
                    Err(ParseError {
                        expected: expected,
                        recoverable: false,
                        inner_error: ChoiceError::Unrecoverable,
                    })
                }
            }
        }
    }
}

impl<Input, Item> Parser<Input> for Choice<Input, ListOf![Item]>
where
    Input: TokenStream,
    Item: Parser<Input>,
{
    type Output = Item::Output;
    type Error = ChoiceError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let of = self.0;
        let ListPat![parser, .._] = of;
        match parser.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                expected,
                recoverable,
                inner_error: _,
            }) => {
                if recoverable {
                    Err(ParseError {
                        expected: expected,
                        recoverable: true,
                        inner_error: ChoiceError::AllFailed,
                    })
                } else {
                    Err(ParseError {
                        expected: expected,
                        recoverable: false,
                        inner_error: ChoiceError::Unrecoverable,
                    })
                }
            }
        }
    }
}

// pub fn some<I, O, E>(of: impl Parser<I, O, E>) -> impl Parser<I, O, SomeError<E>> {
//     Some { of }
// }

// pub enum SomeError<E> {}

// struct Some<I, O, E> {
//     pub of: dyn Parser<I, O, E>,
// }

// impl<I, O, E> Parser<I, Vec<O>, SomeError<E>> for Some<I, O, E> {
//     fn parse(
//         &self,
//         input: impl TokenStream<I>,
//     ) -> Result<ParseSuccess<impl TokenStream<I>, Vec<O>>, ParseError<SomeError<E>>> {
//         let mut res = Vec::new();
//         while let Ok(inner_res) = self.of.parse(input) {
//             res.push(inner_res);
//         }
//         Ok(res)
//     }
// }

// pub struct Produce<I, O, E, InnerO> {
//     value: O,
//     when: dyn Parser<I, InnerO, E>,
// }

// impl<I, O, E, InnerO> Parser<I, O, E> for Produce<I, O, E, InnerO> {
//     fn parse(&mut self, input: impl TokenStream<I>) {
//         self.when.parse(input)?;
//         Ok(self.value)
//     }
// }

// pub struct Sequence<I, O, E> {
//     of: Vec<dyn Parser<I, O, E>>,
// }

// impl<I, O, E> Parser<I, Vec<Result<O, E>>, E> for Sequence<I, O, E> {
//     fn parse(&mut self, input: impl TokenStream<I>) {
//         let mut res = Vec::new();
//         for parser in self.parsers {
//             let inner_res = parser(input)?;
//             res.push(inner_res);
//         }
//         Ok(res)
//     }
// }
