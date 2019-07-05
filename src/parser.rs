use crate::lexer::{Lexer, Token, Operator, ParseError};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

#[macro_export]
macro_rules! map {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    precedences: HashMap<Operator, (u8, Associativity)>
}

#[derive(Debug, Clone, PartialEq)]
enum Associativity {
    Left,
    Right
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Parser {
        Parser {
            lexer: Lexer::new(src),
            precedences: map!(
                Operator::Add => (2, Associativity::Left),
                Operator::Sub => (2, Associativity::Left),
                Operator::Mul => (3, Associativity::Left),
                Operator::Div => (3, Associativity::Left),
                Operator::Pow => (4, Associativity::Right)
            )
        }
    }

    pub fn compute(&mut self) -> Result<f64, ParseError> {
        let mut stack: Vec<f64> = Vec::new();
        let rpn_stream = self.to_rpn()?;
        for token in rpn_stream {
            match token {
                Token::Number(n) => stack.push(n),
                Token::Operator(op) => {
                    // WIP: remove these ugly match arms
                    let n1 = stack.pop().ok_or(ParseError::InvalidExpression)?;
                    let n2 = stack.pop().ok_or(ParseError::InvalidExpression)?;
                    // WIP: a better way to do this
                    let result: f64;
                    match op {
                        Operator::Add => {
                            result = n2 + n1;
                        }
                        Operator::Mul => {
                            result = n2 * n1;
                        }
                        Operator::Sub => {
                            result = n2 - n1;
                        }
                        Operator::Div => {
                            result = n2 / n1;
                        }
                        Operator::Pow => {
                            result = n2.powf(n1)
                        }
                        _ => unreachable!()
                    }
                    stack.push(result);
                }
                _ => unreachable!()
            }
        };
        Ok(stack.pop().unwrap())
    }

    fn to_rpn(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut output: Vec<Token> = Vec::new();
        let mut stack: Vec<Token> = Vec::new();

        use Token::*;
        loop {
            let token = self.lexer.lex()?;
            match token {
                Number(_) => output.push(token),
                Operator(op1) => {
                    while stack.len() > 0 {
                        let (o1_prec, o1_assoc) = self.precedences.get(&op1).unwrap();
                        match stack.last() {
                            Some(Operator(op2)) => {
                                let (o2_prec,_) = self.precedences.get(op2).unwrap();
                                if (*o1_assoc == Associativity::Left &&
                                    o1_prec <= o2_prec) ||
                                    (*o1_assoc == Associativity::Right &&
                                    o1_prec < o2_prec) {
                                        output.push(stack.pop().unwrap());
                                } else {
                                    break
                                }
                            }
                            _ => break
                        }
                    }
                    stack.push(token.clone());
                },
                LeftParen => stack.push(token),
                RightParen => {
                    loop {
                        match stack.last() {
                            Some(&LeftParen) => {
                                stack.pop().unwrap();
                                break;
                            }
                            None => {
                                return Err(ParseError::UnbalancedParens);
                            }
                            _ => output.push(stack.pop().unwrap())
                        }
                    }
                }
                EOF => {
                    // pop entire stack to output
                    while stack.len() > 0 {
                        let op = stack.pop();
                        match op {
                            Some(LeftParen) => (),
                            Some(RightParen) => (),
                            _ => output.push(op.unwrap())
                        }
                    }
                    break;
                }
                _ => unreachable!()
            }
        }

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rpn() {
        let mut parser = Parser::new("3 + (2 ^ 3)");
        let postfix = parser.to_rpn().unwrap();
        // transforms this to postfix, so check operator positions
        assert_eq!(postfix.len(), 5);
        assert_eq!(postfix[3], Token::Operator(Operator::Pow));
        assert_eq!(postfix[4], Token::Operator(Operator::Add));
    }

    #[test]
    fn test_compute() {
        let mut parser = Parser::new("3 + 4 * (2 - 1)");
        let result = parser.compute().unwrap();
        assert_eq!(result, 7f64);
    }
}