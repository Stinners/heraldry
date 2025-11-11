#![allow(dead_code)]

use std::sync::Arc;

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
pub enum ProjectionBind {
    STAR(Loc), 
    EXPRESSION(Expression),
}

pub type Projection = (ProjectionBind, Option<Arc<str>>);

#[derive(Debug)]
pub struct FromClause {
    pub table: Expression,
    pub joins: Vec<Join>,
}

#[derive(Debug)]
pub struct SelectStmt {
    pub proj: Vec<Projection>,
    pub from: Option<FromClause>,
    pub _where: Option<Expression>,
    pub having: Option<Expression>,
    pub qualify: Option<Expression>,
    pub span: Loc,
}   
