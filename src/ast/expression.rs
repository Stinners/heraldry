#![allow(dead_code)]

use std::boxed::Box;

use crate::token::Token;
use crate::loc::Loc;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Expression {
    IDENT(Token),
    COMPOUND_IDENT(Vec<Token> , Loc),
    UNIARY(Token, Box<Expression>),
    BINARY(Box<Expression>, Token, Box<Expression>),
}

impl Expression {
    fn span(&self) -> Loc {
        use Expression::*;
        match self {
            IDENT(token) => token.span,
            COMPOUND_IDENT(_, span) => *span,
            UNIARY(op, exp) => op.span.combine(&exp.span()),
            BINARY(left, _, right) => left.span().combine(&right.span()),
        }
    }
}

