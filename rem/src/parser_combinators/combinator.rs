use super::{ParseError, ParseSuccess, Parser, TokenStream};

use crate::types::list::{Cons, List, Nil};

#[derive(Clone, PartialEq)]
pub struct Choice<Parsers>(pub Parsers)
where
    Parsers: List;

#[derive(Debug, Eq, PartialEq)]
pub enum ChoiceError {
    AllFailed,
    Unrecoverable,
}

impl<Input, Item, Rest> Parser<Input> for Choice<Cons<Item, Rest>>
where
    Input: TokenStream,
    Item: Parser<Input> + Clone + PartialEq,
    Rest: List,
{
    type Output = <Item as Parser<Input>>::Output;
    type Error = ChoiceError;

    fn parse(
        &self,
        input: Input,
    ) -> Result<ParseSuccess<Self::Output, Input>, ParseError<Self::Error>> {
        let Choice(between) = self;
        let Cons(parser, rest) = between;
        match parser.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                expected,
                recoverable,
                inner_error,
            }) => {
                if recoverable {
                    Choice(rest.clone()).parse(input)
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

impl<Input> Parser<Input> for Choice<Nil>
where
    Input: TokenStream,
{
    type Output = ();
    type Error = ChoiceError;

    fn parse(
        &self,
        _input: Input,
    ) -> Result<ParseSuccess<Self::Output, Input>, ParseError<Self::Error>> {
        Err(ParseError {
            expected: "".to_string(),
            recoverable: true,
            inner_error: ChoiceError::AllFailed,
        })
    }
}

// trait ChoiceCombinator<Input, Parsers>
// where
//     Input: TokenStream,
//     Parsers: List,
// {
//     fn choice(&self, between: Parsers) -> impl Parser<Input>;
// }

// impl<Input, Parsers> ChoiceCombinator<Input, Parsers> for Choice<Parsers>
// where
//     Input: TokenStream,
//     Parsers: List,
// {
//     fn choice(&self, between: Parsers) -> impl Parser<Input> {

//     }
// }

// impl<Input> ChoiceCombinator<Input, Nil> for Nil
// where
//     Input: TokenStream,
// {
//     fn choice(&self, between: Nil) -> impl Parser<Input> {
//         "".to_string()
//     }
// }

// impl<Input, Item, Rest> ChoiceCombinator<Input, Cons<Item, Rest>> for Cons<Item, Rest>
// where
//     Input: TokenStream,
// {
//     fn choice(&self, between: Cons<Item, Rest>) -> impl Parser<Input> {
//         todo!()
//     }
// }

// pub fn choice<Parsers>(between: Parsers) -> Choice<Parsers>
// where
//     Parsers: List,
// {
//     Choice { between }
// }

// #[derive(Debug, Eq, PartialEq)]
// pub enum ChoiceError {
//     Unrecoverable,
//     AllFailed,
// }

// struct Choice<Parsers> {
//     pub between: Parsers,
// }

// impl<Input, Output> Parser<Input> for Choice<Input, Output> {
//     type Output = Output;
//     type Error = ChoiceError;

//     fn parse(
//         &self,
//         input: Input,
//     ) -> Result<ParseSuccess<Input, Self::Output>, ParseError<Self::Error>> {
//         let mut expecteds: Vec<String>;
//         for parser in self.between {
//             match parser.parse(input) {
//                 Ok(res) => {
//                     return Ok(res);
//                 }
//                 Err(err) => {
//                     if !err.recoverable {
//                         return Err(ParseError {
//                             expected: err.expected,
//                             recoverable: false,
//                             inner_error: ChoiceError::Unrecoverable,
//                         });
//                     }
//                     expecteds.push(err.expected)
//                 }
//             }
//         }
//         Err(ParseError {
//             expected: expecteds.join(" or "),
//             recoverable: true,
//             inner_error: ChoiceError::AllFailed,
//         })
//     }
// }

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
