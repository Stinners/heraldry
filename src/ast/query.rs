#![allow(dead_code)]

use crate::ast::Expression;
use crate::loc::Loc;

#[derive(Debug)]
pub enum JoinType {
    Inner,
    Left, 
    Right,
    Cross,
}

#[derive(Debug)]
pub struct Join {
    pub join_type: JoinType,
    pub right_table: Expression,
}

#[derive(Debug)]
pub enum Projection {
    STAR(Loc), 
    EXPRESSION(Expression),
}

#[derive(Debug)]
pub struct FromClause {
    table: Expression,
    joins: Vec<Join>,
}

#[derive(Debug)]
pub struct SelectStmt {
    proj: Vec<Projection>,
    from: Option<FromClause>,
    _where: Option<Expression>,
    having: Option<Expression>,
    qualify: Option<Expression>
}
