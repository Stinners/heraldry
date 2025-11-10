#![allow(dead_code)]

use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;

use thiserror::Error;

use crate::token::{Token, TokenT};
use crate::loc::Loc;


const KEYWORD_MAP: &[(&str, TokenT)] = &[
    ("select", TokenT::SELECT), 
    ("from", TokenT::FROM),
    ("where", TokenT::WHERE),
    ("join", TokenT::JOIN)
];

// Char identifier functions

fn is_identifier_inner(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '&'
}

fn is_identifier_first(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}


#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Unterminated string")]
    UnterminatedString(Loc),
    #[error("Unexpected Character")]
    UnexpectedCharacter(char, Loc),
    #[error("Unterminated Macro")]
    UnterminatedMacro(Loc),
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    pub text: Arc<str>,
    pub span: Loc,
}

impl<'a> Lexer<'a> {
    fn get_text(&self) -> &str {
        &self.text[self.span.start..self.span.end]
    }

    fn emit(&mut self, kind: TokenT) -> Result<Token, LexerError> {
        let old_span = self.span;
        self.span = old_span.start_new();
        Ok(Token {
            kind,
            span: old_span,
            text: self.text.clone(),
        })
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|c| {
            self.span.advance(c);
            c
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn consume_if(&mut self, func: fn(char) -> bool) -> bool {
        if self.peek().is_some_and(func) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    fn consume_while(&mut self, func: fn(char) -> bool) {
        while self.peek().is_some_and(func) {
            let _ = self.advance();
        }
    }

    // There are faster ways to do this
    fn keyword_type(&self) -> TokenT {
        let text = self.get_text();

        if text.eq_ignore_ascii_case("true") {
            return TokenT::BOOLEAN(true)
        }
        else if text.eq_ignore_ascii_case("false") {
            return TokenT::BOOLEAN(false)
        }

        for (word, token_t) in KEYWORD_MAP.iter() {
            if text.eq_ignore_ascii_case(word) {
                return *token_t;
            };
        }
        TokenT::IDENTIFIER
    }

    fn consume_identifier_or_keyword(&mut self) {
        self.consume_while(is_identifier_inner);
    }

    fn consume_string(&mut self) -> Result<(), LexerError> {
        dbg!("Consuming string'");
        while let Some(c) = self.advance() {
            if c == '\'' {
                return Ok(());
            }
            // Escaping single quotes 
            else if c == '\\' && self.peek() == Some('\'') {
                self.advance();
            }
        }
        Err(LexerError::UnterminatedString(self.span))
    }

    fn consume_number(&mut self, c: char) {
        let mut seen_dot = c == '.';

        // Consume the significand 
        while let Some(c) = self.peek() {
            if c == '.' && !seen_dot {
                seen_dot = true;
            }
            else if !c.is_numeric() {
                break;
            }
            let _ = self.advance();
        }

        // Consume an optional exponent
        if self.consume_if(|c| c == 'e' || c == 'E') {
            self.consume_if(|c| c == '-' || c == '+');
            self.consume_while(|c| c.is_numeric());
        }
    }

    // TODO handle python strings
    fn consume_macro(&mut self) -> Result<(), LexerError> {
        while let Some(c) = self.advance() {
            if c == '}' && self.consume_if(|c| c == '}') {
                return Ok(());
            }
        }
        Err(LexerError::UnterminatedMacro(self.span))
    }

    fn lex_number(&mut self, next_c: char) -> Result<Token, LexerError> {
        self.consume_number(next_c);
        let value = self.get_text().parse().expect("Only lex valid numbers");
        self.emit(TokenT::NUMBER(value))
    }

    pub fn lex_token(&mut self) -> Result<Token, LexerError> {
        // Loop until we either consume leading whitespace or reach the end of the file
        let next_c = loop {
            if let Some(next_c) = self.advance() {
                if !next_c.is_whitespace() {
                    break next_c;
                }
                else {
                    self.span.advance_start(next_c);
                }
            } 
            else {
                return self.emit(TokenT::EOF);
            }
        };

        let token = match next_c {
            // Single character Tokens
            '*' => self.emit(TokenT::STAR),
            ';' => self.emit(TokenT::SEMICOLON),
            ',' => self.emit(TokenT::COMMA),
            '(' => self.emit(TokenT::L_PAREN),
            ')' => self.emit(TokenT::R_PAREN),
            '[' => self.emit(TokenT::L_SQUARE),
            ']' => self.emit(TokenT::R_SQUARE),
            '}' => self.emit(TokenT::R_CURLY),
            '=' => self.emit(TokenT::EQ),

            // Double Character Tokens
            '.' => { 
                if self.peek().is_some_and(|c| c.is_numeric()) {
                    self.lex_number(next_c)
                } 
                else {
                    self.emit(TokenT::DOT)
                }
            }

            '<' => { 
                if self.consume_if(|c| c == '=') { self.emit(TokenT::LT_EQ) }
                else if self.consume_if(|c| c == '>') { self.emit(TokenT::N_EQ) }
                else { self.emit(TokenT::LT) }
            }

            '>' => { 
                if self.consume_if(|c| c == '=') { self.emit(TokenT::GT_EQ) }
                else { self.emit(TokenT::GT) }
            }

            '!' => {
                if self.consume_if(|c| c == '=') { self.emit(TokenT::N_EQ) }
                else { self.emit(TokenT::BANG) }
            }

            '\'' => {
                self.consume_string()?;
                self.emit(TokenT::STRING)
            }

            '{' => {
                if self.consume_if(|c| c == '{') { 
                    self.consume_macro()?;
                    self.emit(TokenT::MACRO)
                }
                else {
                    self.emit(TokenT::L_CURLY)
                }
            }

            // More complicated matches
            _ => {
                if is_identifier_first(next_c) {
                    self.consume_identifier_or_keyword();
                    let token_t = self.keyword_type();
                    self.emit(token_t)
                }

                else if next_c.is_numeric() {
                    self.lex_number(next_c)
                }

                else {
                    return Err(LexerError::UnexpectedCharacter(next_c, self.span));
                }
            }
        };

        token
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_token() {
            Ok(token) if token.kind != TokenT::EOF => Some(token),
            _ => None
        }
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(text: &'a str) -> Lexer<'a> {
        Lexer {
            chars: text.chars().peekable(),
            text: text.into(),
            span: Loc::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenT::*;

    fn make_tokens(text: &str) -> Vec<Token> {
        Lexer::from(text).into_iter().collect()
    }

    fn compare_kinds(expected: &[TokenT], actual: &[Token]) {
        assert_eq!(expected.len(), actual.len());

        for (actual, expected) in expected.iter().zip(actual) {
            assert_eq!(*actual, expected.kind);
        }
    }

    #[test]
    fn handles_empty_string() {
        let tokens = make_tokens("");
        assert!(tokens.len() == 0);
    }

    #[test]
    fn lex_keywords() {
        let tokens = make_tokens("  select  SELECT From foo ");
        compare_kinds(&[SELECT, SELECT, FROM, IDENTIFIER], &tokens);
    }

    #[test]
    fn lex_punctuation() {
        let tokens = make_tokens("*; , . = >");
        compare_kinds(&[STAR, SEMICOLON, COMMA, DOT, EQ, GT], &tokens);
    }

    #[test]
    fn lex_two_character_symbols() {
        let tokens = make_tokens("!= <> <= >=");
        compare_kinds(&[N_EQ, N_EQ, LT_EQ, GT_EQ], &tokens);
    }

    #[test]
    fn lex_simple_select_punctuation() {
        let tokens = make_tokens("select *, bar from foo;");
        compare_kinds(&[SELECT, STAR, COMMA, IDENTIFIER, FROM, IDENTIFIER, SEMICOLON], &tokens);
    }

    #[test] 
    fn lex_identifiers() {
        let tokens = make_tokens("foo bar");
        compare_kinds(&[IDENTIFIER, IDENTIFIER], &tokens);
        assert_eq!(tokens[0].get_str(), "foo");
        assert_eq!(tokens[1].get_str(), "bar");
    }

    #[test] 
    fn lex_integers() {
        let tokens = make_tokens("1 10 1000");
        compare_kinds(&[NUMBER(1.0), NUMBER(10.0), NUMBER(1000.0)], &tokens);
    }

    #[test] 
    fn lex_floats() {
        let tokens = make_tokens("1.0 10.0 .1 1e2 .1e-1");
        compare_kinds(&[NUMBER(1.0), NUMBER(10.0), NUMBER(0.1), NUMBER(100.0), NUMBER(0.01)], &tokens);
    }

    #[test] 
    fn lex_bools() {
        let tokens = make_tokens("true false TRUE");
        compare_kinds(&[BOOLEAN(true), BOOLEAN(false), BOOLEAN(true)], &tokens);
    }

    #[test] 
    fn lex_strings() {
        let tokens = make_tokens(r"'' 'foo' 'bar\' foo'");
        compare_kinds(&[STRING, STRING, STRING], &tokens);

        let expected_contents = &["", "foo", r"bar\' foo"];
        for (token, expected_content) in tokens.iter().zip(expected_contents) {
            let actual_content = token.get_string_content();
            assert_eq!(*expected_content, actual_content);
        }
    }

    #[test] 
    fn lex_macro() {
        let tokens = make_tokens("{{ foo , + daww a }}");
        compare_kinds(&[MACRO], &tokens);
    }
}
