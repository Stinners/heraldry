#![allow(dead_code)]

use std::iter::Peekable;

use crate::token::{Token, TokenT};
use crate::loc::Loc;
use crate::lexer::Lexer;
use crate::ast::*;

#[derive(Debug)]
enum ParseErrorT {
    EOF, 
    UnexpectedToken,
}

#[derive(Debug)]
struct ParseError {
    msg: String,
    span: Loc,
    kind: ParseErrorT,
}

impl ParseError {
    fn eof<R>(loc: Loc) -> Result<R, Self> {
        Err(ParseError {
            msg: "Unexpected end of file".into(),
            span: loc,
            kind: ParseErrorT::EOF

        })
    }

    fn unexpected<R>(expected: TokenT, actual: Token) -> Result<R, Self> {
        Err(ParseError {
            msg: format!("Expected {:?}, but got {:?}", expected, actual.kind),
            span: actual.span,
            kind: ParseErrorT::UnexpectedToken,
        })
    }

    fn elaborate(&mut self, text: &str) {
        self.msg.push_str(" ");
        self.msg.push_str(text);
    }

    fn replace(mut self, text: &str) -> ParseError {
        self.msg = text.to_string();
        self
    }
}

struct ParserSettings {
    allow_trailing_commas: bool,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    settings: ParserSettings,
    span: Loc,
}

impl<'a> Parser<'a> {

    fn new(lexer: Lexer<'a>, settings: ParserSettings) -> Self {
        Parser {
            span: lexer.span,
            lexer: lexer.peekable(),
            settings,
        }
    }

    // =============== Helper Functions =============== */

    fn next(&mut self) -> Result<Token, ParseError> {
        match self.lexer.next() {
            Some(token) => {
                self.span = token.span;
                Ok(token)
            }
            None => ParseError::eof(self.span)
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    // Advances only of the token matches
    fn match_token(&mut self, token_type: TokenT) -> Option<Token> {
        match self.peek() {
            Some(token) if token.kind == token_type => {
                Some(self.next().unwrap())
            }
            _ => None
        }
    }

    fn match_one_of(&mut self, token_types: &[TokenT]) -> Option<Token> {
        match self.peek() {
            None => None,
            Some(token) => {
                for token_type in token_types {
                    if token.kind == *token_type {
                        return Some(self.next().unwrap());
                    }
                }
                None
            }
        }
    }


    // TODO, these functions should peek and only consume if they match
    fn require_token(&mut self, token_type: TokenT) -> Result<Token, ParseError> {
        match self.peek() {
            None => ParseError::eof(self.span),
            Some(token) if token.kind == token_type => {
                self.next()
            }
            Some(token) => ParseError::unexpected(token_type, token.clone())
        }
    }


    fn require_one_of(&mut self, token_types: &[TokenT]) -> Result<Token, ParseError> {
        match self.peek() {
            None => ParseError::eof(self.span),
            Some(token) => {
                if token_types.iter().any(|t| *t == token.kind) {
                    self.next()
                }
                else {
                    Err(ParseError {
                        msg: format!("Expected one of {:?}, but got {:?}", token_types, token.kind),
                        span: token.span,
                        kind: ParseErrorT::UnexpectedToken,
                    })
                }
            }
        }
    }

    // =============== Parsing Functions =============== */

    fn parse_projection(&mut self) -> Result<Projection, ParseError> {
        let token = self.require_one_of(&[TokenT::STAR, TokenT::IDENTIFIER])?;

        Ok(match token.kind {
            TokenT::STAR => Projection::STAR(token.span),
            TokenT::IDENTIFIER => Projection::EXPRESSION(Expression::IDENT(token)),
            _ => unreachable!("Unexpected token type in parse_projection")
        })
    }

    fn parse_projections(&mut self) -> Result<Vec<Projection>, ParseError> {
        let mut projections = vec!();

        // We must parse at least one projection 
        match self.parse_projection() {
            Ok(proj) => projections.push(proj),
            Err(mut err) => {
                err.elaborate("Expected at least one projection");
                return Err(err);
            }
        }

        // Then we can parse any number of commas followed by projections
        loop {
            // Consume a comma and break if it's not one 
            if self.match_token(TokenT::COMMA).is_none() {
                break;
            }

            match self.parse_projection() {
                Ok(proj) => projections.push(proj),
                Err(_) if self.settings.allow_trailing_commas => {
                    break;
                },
                Err(err) => { return Err(err) }
            }
        }
        Ok(projections)
    }

    fn parse_from(&mut self) -> Result<Expression, ParseError> {
        let _ = self.require_token(TokenT::FROM)?;
        let table = self.require_token(TokenT::IDENTIFIER)?;
        Ok(Expression::IDENT(table))
    }

    fn parse_select(&mut self) -> Result<SelectClause, ParseError> {
        let span = self.span;
        let _ = self.require_token(TokenT::SELECT)?;
        let projections = self.parse_projections()?;
        let from = self.parse_from()?;

        Ok(SelectClause {
            projs: projections,
            table: from,
            span
        })

    }
}

