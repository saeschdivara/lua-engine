use std::iter::{Cloned, Enumerate};
use std::slice::Iter;

use nom::{Compare, CompareResult, InputIter, InputLength, InputTake, Needed, UnspecializedInput};

use crate::ast::{Token, TokenStream};

impl InputLength for TokenStream {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl InputTake for TokenStream {
    fn take(&self, count: usize) -> Self {
        todo!()
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        (
            TokenStream {
                tokens: Vec::from(prefix)
            },
            TokenStream {
                tokens: Vec::from(suffix)
            }
        )
    }
}

impl UnspecializedInput for TokenStream {}

impl<'a> InputIter for &'a TokenStream {
    type Item = Token;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Cloned<Iter<'a, Token>>;

    fn iter_indices(&self) -> Self::Iter {
        todo!()
    }

    fn iter_elements(&self) -> Self::IterElem {
        todo!()
    }

    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::Item) -> bool {
        todo!()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        todo!()
    }
}

impl Compare<Token> for TokenStream {
    fn compare(&self, t: Token) -> CompareResult {
        let pos = self.tokens.first();

        match pos {
            Some(first_token) => {
                if std::mem::discriminant(&t) == std::mem::discriminant(first_token) {
                    CompareResult::Ok
                } else {
                    CompareResult::Error
                }
            }
            None => CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: Token) -> CompareResult {
        todo!()
    }
}

impl InputLength for Token {
    fn input_len(&self) -> usize {
        1
    }
}

impl InputTake for Token {
    fn take(&self, count: usize) -> Self {
        todo!()
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        todo!()
    }
}