
use std::sync::Arc;

use crate::loc::Loc;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum TokenT {
    // Keywords
    SELECT, FROM, WHERE, JOIN,

    // Symbols
    STAR,
    SEMICOLON, COMMA, DOT, BANG,
    L_PAREN, R_PAREN, L_CURLY, R_CURLY, L_SQUARE, R_SQUARE,
    EQ, N_EQ, GT_EQ, LT_EQ, GT, LT,

    // Literals 
    NUMBER(f64), // We don't care about the difference between ints and floats
    BOOLEAN(bool),
    STRING,      // Get get the contents of the string from the enclosing token
    
    // At this level we don't care about the contents of macros
    MACRO,

    // Identifer
    IDENTIFIER,

    // End of File
    EOF,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenT,
    pub span: Loc,
    pub text: Arc<str>,
}

#[allow(dead_code)]
impl Token {
    pub fn get_str<'a>(&'a self) -> &'a str {
        &self.text[self.span.start..self.span.end]
    }

    pub fn get_string_content<'a>(&'a self) -> &'a str {
        assert!(self.kind == TokenT::STRING);

        // Exclude the quotes
        let start = self.span.start + 1;
        let end = self.span.end - 1;
        let text = &self.text[start..end];
        return text
    }
}
