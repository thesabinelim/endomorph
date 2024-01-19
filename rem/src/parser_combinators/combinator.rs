// use core::fmt::Debug;
// use std::any::Any;

// use super::{ParseError, ParseSuccess, Parser, TokenStream};

// pub fn choice<Input, Output>(
//     between: Vec<dyn Parser<Input, Output = Output, Error = dyn Any>>,
// ) -> impl Parser<Input, Output = Output, Error = ChoiceError> {
//     Choice { between }
// }

// #[derive(Debug, Eq, PartialEq)]
// pub enum ChoiceError {
//     Unrecoverable,
//     AllFailed,
// }

// struct Choice<Input, Output> {
//     pub between: Vec<dyn Parser<Input, Output = Output, Error = dyn Any>>,
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
