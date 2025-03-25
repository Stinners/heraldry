
#![allow(dead_code)]

use std::iter::Peekable;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};
use std::sync::Arc;


#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Loc {
    start: usize,
    end: usize,
    line: usize,
}

impl Loc {
    fn new() -> Self {
        Loc {
            start: 0,
            end: 0,
            line: 1,
        }
    }

    fn advance(&mut self, cluster: &str) {
        self.end = self.end + cluster.len()
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TokenT {
    // Single word tokens
    SELECT, FROM,

    // Symbols
    STAR, SEMICOLON,

    // Identifer
    IDENTIFIER,
}

const KEYWORD_MAP: &[(&str, TokenT)] = 
    &[("select", TokenT::SELECT),
      ("from", TokenT::FROM),
    ];


#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    kind: TokenT,
    span: Loc,
    text: Arc<str>
}

impl Token {
    fn get_str<'a>(&'a self) -> &'a str {
        &self.text[self.span.start..self.span.end]
    }
}


#[derive(Debug)]
pub struct Lexer<'a> {
    graphs: Peekable<Graphemes<'a>>,
    text: Arc<str>,
    span: Loc,
}

impl<'a> Lexer<'a> {
    fn lex_string(string: &'a str) -> Self {
        Lexer {
            graphs: UnicodeSegmentation::graphemes(string, true).peekable(),
            text: string.into(),
            span: Loc::new(),
        }
    }

    fn get_text(&self) -> &str {
        &self.text[self.span.start..self.span.end]
    }

    fn emit(& mut self, kind: TokenT) -> Token {
        let old_span = self.span;
        self.span.start = self.span.end;
        Token {
            kind,
            span: old_span,
            text: self.text.clone()
        }
    }

    fn advance(&mut self) -> Option<&str> {
        match self.graphs.next() {
            Some(cluster) => {
                self.span.advance(cluster);
                Some(cluster)
            },
            None => None,
        }
    }


    fn peek(&mut self) -> Option<&str> {
        self.graphs.peek().map(|c| &**c)
    }

    // We could do this faster with a trie 
    fn keyword_type(&self) -> TokenT {
        let text = self.get_text();
        for (word, token_t) in KEYWORD_MAP.iter() {
            if text.eq_ignore_ascii_case(word) {
                return *token_t;
            };
        }
        TokenT::IDENTIFIER
    }


    pub fn lex(&mut self) -> Option<Token> {
        let next_c = self.advance();
        if next_c.is_none() {
            return None;
        }

        let token = match next_c.unwrap() {
            // Single character Tokens 
            "*" => self.emit(TokenT::STAR),
            ";" => self.emit(TokenT::SEMICOLON),

            // Double Character Tokens 

            // Keywords and identifiers 
            _keyword_or_identifier => {
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


#[cfg(test)] 
mod tests {
    use super::*;
}
