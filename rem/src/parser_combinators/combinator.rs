use core::fmt::Debug;
use core::marker::PhantomData;

use super::{LikeParserList, ParseError, Parser, TokenStream};
use crate::types::list::{ListOf, ListPat, NonEmptyList};

#[derive(Clone, PartialEq)]
pub struct Or<Input, Output, Parsers>(Parsers, PhantomData<Input>, PhantomData<Output>)
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output> + NonEmptyList;

impl<Input, Output, Parsers> Or<Input, Output, Parsers>
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub fn of(parsers: Parsers) -> Self {
        Or(parsers, PhantomData, PhantomData)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum OrError {
    AllFailed,
    Unrecoverable,
}

impl<Input, Output, Item, Rest> Parser<Input> for Or<Input, Output, ListOf![Item, ..Rest]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output> + NonEmptyList,
    Or<Input, Output, Rest>: Parser<Input, Output = Output>,
{
    type Output = Output;
    type Error = OrError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, ..rest] = &self.0;
        match parser.parse(input.clone()) {
            Ok(result) => Ok(result),
            Err(ParseError {
                recoverable,
                inner_error: _,
            }) => {
                if recoverable {
                    match Or::of(rest.clone()).parse(input) {
                        Ok(result) => Ok(result),
                        Err(ParseError {
                            recoverable,
                            inner_error: _,
                        }) => Err(if recoverable {
                            ParseError {
                                recoverable,
                                inner_error: OrError::AllFailed,
                            }
                        } else {
                            ParseError {
                                recoverable,
                                inner_error: OrError::Unrecoverable,
                            }
                        }),
                    }
                } else {
                    Err(ParseError {
                        recoverable,
                        inner_error: OrError::Unrecoverable,
                    })
                }
            }
        }
    }
}

impl<Input, Output, Item> Parser<Input> for Or<Input, Output, ListOf![Item]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
{
    type Output = Output;
    type Error = OrError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, .._] = &self.0;
        match parser.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                recoverable,
                inner_error: _,
            }) => Err(if recoverable {
                ParseError {
                    recoverable,
                    inner_error: OrError::AllFailed,
                }
            } else {
                ParseError {
                    recoverable,
                    inner_error: OrError::Unrecoverable,
                }
            }),
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

#[derive(Clone, PartialEq)]
pub struct Sequence<Input, Output, Parsers>(Parsers, PhantomData<Input>, PhantomData<Output>)
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output> + NonEmptyList;

impl<Input, Output, Parsers> Sequence<Input, Output, Parsers>
where
    Input: TokenStream,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub fn of(parsers: Parsers) -> Self {
        Sequence(parsers, PhantomData, PhantomData)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SequenceError {
    Failed,
    Unrecoverable,
}

impl<Input, Output, Item, Rest> Parser<Input> for Sequence<Input, Output, ListOf![Item, ..Rest]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output> + NonEmptyList,
    Sequence<Input, Output, Rest>: Parser<Input, Output = Vec<Output>>,
{
    type Output = Vec<Output>;
    type Error = SequenceError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, ..rest] = &self.0;
        match parser.parse(input.clone()) {
            Ok((output, next_input)) => match Sequence::of(rest.clone()).parse(next_input) {
                Ok((rest_output, next_input)) => {
                    Ok((vec![vec![output], rest_output].concat(), next_input))
                }
                Err(ParseError {
                    recoverable,
                    inner_error: _,
                }) => Err(if recoverable {
                    ParseError {
                        recoverable,
                        inner_error: SequenceError::Failed,
                    }
                } else {
                    ParseError {
                        recoverable,
                        inner_error: SequenceError::Unrecoverable,
                    }
                }),
            },
            Err(ParseError {
                recoverable,
                inner_error: _,
            }) => Err(if recoverable {
                ParseError {
                    recoverable,
                    inner_error: SequenceError::Failed,
                }
            } else {
                ParseError {
                    recoverable,
                    inner_error: SequenceError::Unrecoverable,
                }
            }),
        }
    }
}

impl<Input, Output, Item> Parser<Input> for Sequence<Input, Output, ListOf![Item]>
where
    Input: TokenStream,
    Output: Clone + PartialEq + Debug,
    Item: Parser<Input, Output = Output>,
{
    type Output = Vec<Output>;
    type Error = SequenceError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let ListPat![parser, .._] = &self.0;
        match parser.parse(input) {
            Ok((output, next_input)) => Ok((vec![output], next_input)),
            Err(ParseError {
                recoverable,
                inner_error: _,
            }) => Err(if recoverable {
                ParseError {
                    recoverable,
                    inner_error: SequenceError::Failed,
                }
            } else {
                ParseError {
                    recoverable,
                    inner_error: SequenceError::Unrecoverable,
                }
            }),
        }
    }
}

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

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        match self.0.parse(input) {
            Ok(result) => Ok(result),
            Err(ParseError {
                recoverable: _,
                inner_error,
            }) => Err(ParseError {
                recoverable: false,
                inner_error,
            }),
        }
    }
}
