mod expression;
mod query;

pub use crate::ast::expression::Expression;
pub use crate::ast::query::*;

#[cfg(test)]
mod tests {
    pub trait SExp {
        fn s_exp(&self) -> String;
    }
}
