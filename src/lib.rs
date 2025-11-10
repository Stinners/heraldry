
mod loc;
mod token;
mod lexer;
mod ast;
mod parser;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
}
