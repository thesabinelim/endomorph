use core::fmt::Debug;
use core::marker::PhantomData;

use super::{LikeParserList, ParseError, Parser, TokenStream};
use crate::types::list::{ListCons, ListOf, ListPat};

#[derive(Clone, PartialEq)]
pub struct Choice<Input, Output, Parsers>(Parsers, PhantomData<Input>, PhantomData<Output>)
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output>;

impl<Input, Output, Parsers> Choice<Input, Output, Parsers>
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output>,
{
    pub fn of(parsers: Parsers) -> Choice<Input, Output, Parsers> {
        Choice(parsers, PhantomData, PhantomData)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ChoiceError {
    AllFailed,
    Unrecoverable,
}

impl<Input, Output, Item, Rest> Parser<Input> for Choice<Input, Output, ListOf![Item, ..Rest]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output> + ListCons,
    Choice<Input, Output, Rest>: Parser<Input, Output = Output>,
{
    type Output = Output;
    type Error = ChoiceError;

    fn expected(&self) -> String {
        let ListPat![parser, ..rest] = &self.0;
        let expected = parser.expected();
        let rest_expected = Choice::of(rest.clone()).expected();
        format!("{} or {}", expected, rest_expected)
    }

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, ..rest] = &self.0;
        match parser.parse(input.clone()) {
            Ok(result) => Ok(result),
            Err(ParseError {
                expected,
                recoverable,
                inner_error: _,
            }) => {
                if recoverable {
                    match Choice::of(rest.clone()).parse(input) {
                        Ok(result) => Ok(result),
                        Err(ParseError {
                            expected,
                            recoverable,
                            inner_error: _,
                        }) => {
                            if recoverable {
                                Err(ParseError {
                                    expected: self.expected(),
                                    recoverable: true,
                                    inner_error: ChoiceError::AllFailed,
                                })
                            } else {
                                Err(ParseError {
                                    expected,
                                    recoverable: false,
                                    inner_error: ChoiceError::Unrecoverable,
                                })
                            }
                        }
                    }
                } else {
                    Err(ParseError {
                        expected,
                        recoverable: false,
                        inner_error: ChoiceError::Unrecoverable,
                    })
                }
            }
        }
    }
}

impl<Input, Output, Item> Parser<Input> for Choice<Input, Output, ListOf![Item]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
{
    type Output = Output;
    type Error = ChoiceError;

    fn expected(&self) -> String {
        let ListPat![parser, .._] = &self.0;
        parser.expected()
    }

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, .._] = &self.0;
        match parser.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                expected,
                recoverable,
                inner_error: _,
            }) => {
                if recoverable {
                    Err(ParseError {
                        expected: self.expected(),
                        recoverable: true,
                        inner_error: ChoiceError::AllFailed,
                    })
                } else {
                    Err(ParseError {
                        expected,
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

#[derive(Clone, PartialEq)]
pub struct Unrecoverable<Input, InnerParser>(InnerParser, PhantomData<Input>)
where
    Input: TokenStream,
    InnerParser: Parser<Input>;

impl<Input, InnerParser> Unrecoverable<Input, InnerParser>
where
    Input: TokenStream,
    InnerParser: Parser<Input>,
{
    pub fn of(parser: InnerParser) -> Unrecoverable<Input, InnerParser> {
        Unrecoverable(parser, PhantomData)
    }
}

impl<Input, InnerParser> Parser<Input> for Unrecoverable<Input, InnerParser>
where
    Input: TokenStream,
    InnerParser: Parser<Input>,
{
    type Output = InnerParser::Output;
    type Error = InnerParser::Error;

    fn expected(&self) -> String {
        self.0.expected()
    }

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        match self.0.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                expected,
                recoverable: _,
                inner_error,
            }) => Err(ParseError {
                expected,
                recoverable: false,
                inner_error,
            }),
        }
    }
}
