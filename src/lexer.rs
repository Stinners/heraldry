#![allow(dead_code)]

use std::iter::Peekable;
use std::str::Chars;
use std::sync::Arc;

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Loc {
    start: usize,
    end: usize,
    line: [usize; 2],
    line_start: [usize; 2],
}

impl Loc {
    fn new() -> Self {
        Loc {
            // The first byte of the span 
            start: 0,

            // The final byte of the span 
            end: 0,

            // The line the start and end cursors are one
            line: [1, 1],

            // The byte where the line starts
            line_start: [0,0],
        }
    }

    // Length of the span in bytes - not in chars
    fn len(&self) -> usize {
        self.end - self.start
    }

    fn advance(&mut self, c: char) {
        self.end += c.len_utf8();
        if c == '\n' {
            // Advance the end
            self.line[1] += 1;
            self.line_start[1] = self.end + c.len_utf8();
        }
    }

    #[inline(always)]
    fn debug_assert_zero_length(&self) {
        debug_assert!(self.start == self.end);
        debug_assert!(self.line[0] == self.line[1]);
        debug_assert!(self.line_start[0] == self.line_start[1]);
        debug_assert!(self.len() == 0);
    }

    fn advance_start(&mut self, c: char) {
        let step = c.len_utf8();
        self.start += step;
        if c == '\n' {
            self.line[0] += 1;
            self.line_start[0] += step;
        }
        self.debug_assert_zero_length();
    }

    fn start_new(&self) -> Self {
        let mut new_span = self.clone();
        new_span.start = new_span.end;
        new_span.line[0] = new_span.line[1];
        new_span.line_start[0] = new_span.line_start[1];
        new_span
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TokenT {
    // Single word tokens
    SELECT,
    FROM,

    // Symbols
    STAR,
    SEMICOLON, COMMA,

    // Identifer
    IDENTIFIER,
}

const KEYWORD_MAP: &[(&str, TokenT)] = &[("select", TokenT::SELECT), ("from", TokenT::FROM)];

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenT,
    pub span: Loc,
    pub text: Arc<str>,
}

impl Token {
    fn get_str<'a>(&'a self) -> &'a str {
        &self.text[self.span.start..self.span.end]
    }
}

fn identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn identifier_char(c: char) -> bool {
    c.is_alphabetic() || c.is_numeric() || c == '_'
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

    fn emit(&mut self, kind: TokenT) -> Token {
        let old_span = self.span;
        self.span = old_span.start_new();
        Token {
            kind,
            span: old_span,
            text: self.text.clone(),
        }
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

    // There are faster ways to do this
    fn keyword_type(&self) -> TokenT {
        let text = self.get_text();
        for (word, token_t) in KEYWORD_MAP.iter() {
            if text.eq_ignore_ascii_case(word) {
                return *token_t;
            };
        }
        TokenT::IDENTIFIER
    }

    fn consume_identifier(&mut self) {
        while let Some(c) = self.peek() {
            if !identifier_char(c) {
                break;
            }
            self.advance();
        }
    }

    pub fn lex(&mut self) -> Option<Token> {
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
            // End of file
            else {
                
                return None;
            }
        };

        let token = match next_c {
            // Single character Tokens
            '*' => self.emit(TokenT::STAR),
            ';' => self.emit(TokenT::SEMICOLON),
            ',' => self.emit(TokenT::COMMA),

            // Double Character Tokens

            // Keywords and identifiers
            _keyword_or_identifier => {
                self.consume_identifier();
                let token_t = self.keyword_type();
                self.emit(token_t)
            }
        };

        Some(token)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
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

    fn compare_kinds(expected: &[TokenT], actual: Vec<Token>) {
        assert_eq!(expected.len(), actual.len());

        for (act, exp) in expected.iter().zip(actual) {
            assert_eq!(*act, exp.kind);
        }
    }

    #[test]
    fn handles_empty_string_properly() {
        let tokens = make_tokens("");
        assert!(tokens.len() == 0);
    }

    #[test]
    fn lex_keywords() {
        let tokens = make_tokens("  select  SELECT From foo ");
        compare_kinds(&[SELECT, SELECT, FROM, IDENTIFIER], tokens);
    }

    #[test]
    fn lex_punctuation() {
        let tokens = make_tokens("*; ,");
        compare_kinds(&[STAR, SEMICOLON, COMMA], tokens);
    }

    #[test]
    fn lex_simple_select_punctuation() {
        let tokens = make_tokens("select *, bar from foo;");
        compare_kinds(&[SELECT, STAR, COMMA, IDENTIFIER, FROM, IDENTIFIER, SEMICOLON], tokens);
    }
}
