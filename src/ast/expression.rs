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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::tests::SExp;
    use Expression::*;

    impl SExp for Expression {
        fn s_exp(&self) -> String {
            match self {
                IDENT(token) => token.to_string(),
                UNIARY(op, expression) => format!("({} {})", op.to_string(), expression.s_exp()),
                BINARY(left, op, right) => format!("({} {} {})", op.to_string(), left.s_exp(), right.s_exp()),
                COMPOUND_IDENT(parts, _) => parts.iter().map(|t| t.to_string())
                                                 .fold(String::new(), |a, b| a + "." + &b),
            }
        }
    }
}
