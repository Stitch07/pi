use crate::lexer::{Lexer, Operator, ParseError, Token, UnaryOperator};
use crate::math::factorial;
use std::iter::Peekable;

#[macro_export]
macro_rules! map {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

fn get_precedence(token: &Token) -> (usize, Associativity) {
    use Associativity::*;
    use Operator::*;
    match *token {
        Token::Operator(op) => match op {
            Add => (2, Left),
            Sub => (2, Left),
            Mul => (3, Left),
            Div => (3, Left),
            Pow => (4, Right),
        },
        Token::UnaryOperator(op) => match op {
            UnaryOperator::Sub => (5, Right),
            UnaryOperator::Factorial => (6, Right),
        },
        _ => unreachable!(),
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Parser {
        Parser {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn compute(&mut self) -> Result<f64, ParseError> {
        let mut stack: Vec<f64> = Vec::new();
        let rpn_stream = self.postfix()?;
        for token in rpn_stream {
            match token {
                Token::Number(n) => stack.push(n),
                Token::Operator(op) => {
                    let n1 = stack.pop().ok_or(ParseError::InvalidExpression)?;
                    let n2 = stack.pop().ok_or(ParseError::InvalidExpression)?;
                    use Operator::*;
                    let result = match op {
                        Add => n2 + n1,
                        Sub => n2 - n1,
                        Mul => n2 * n1,
                        Div => n2 / n1,
                        Pow => n2.powf(n1),
                    };
                    stack.push(result);
                }
                Token::UnaryOperator(op) => {
                    let n1 = stack.pop().ok_or(ParseError::InvalidExpression)?;
                    use UnaryOperator::*;
                    let result = match op {
                        Sub => -n1,
                        Factorial => {
                            // factorial on negative numbers is invalid
                            if n1.is_sign_negative() {
                                return Err(ParseError::InvalidExpression);
                            }
                            factorial(n1 as i64) as f64
                        }
                    };
                    stack.push(result);
                }
                _ => unreachable!(),
            }
        }
        Ok(stack.pop().unwrap())
    }

    fn postfix(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut output: Vec<Token> = Vec::new();
        let mut stack: Vec<Token> = Vec::new();

        use Token::*;
        while let Some(token) = self.lexer.next().transpose()? {
            match token {
                Number(_) => output.push(token),
                UnaryOperator(_) | Operator(_) => {
                    while !stack.is_empty() {
                        let (o1_prec, o1_assoc) = get_precedence(&token);
                        match stack.last() {
                            Some(Operator(_)) => {
                                let (o2_prec, _) = get_precedence(&token);
                                if (o1_assoc == Associativity::Left && o1_prec <= o2_prec)
                                    || (o1_assoc == Associativity::Right && o1_prec < o2_prec)
                                {
                                    output.push(stack.pop().unwrap());
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                        }
                    }
                    stack.push(token.clone());
                }
                LeftParen => stack.push(token),
                RightParen => loop {
                    match stack.last() {
                        Some(&LeftParen) => {
                            stack.pop().unwrap();
                            break;
                        }
                        None => {
                            return Err(ParseError::UnbalancedParens);
                        }
                        _ => output.push(stack.pop().unwrap()),
                    }
                },
                EOF => unreachable!(),
            }
        }

        // pop entire stack to output
        while !stack.is_empty() {
            let op = stack.pop();
            match op {
                Some(LeftParen) => (),
                Some(RightParen) => (),
                _ => output.push(op.unwrap()),
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
        let postfix = parser.postfix().unwrap();
        // transforms this to postfix, so check operator positions
        assert_eq!(postfix.len(), 5);
        assert_eq!(postfix[3], Token::Operator(Operator::Pow));
        assert_eq!(postfix[4], Token::Operator(Operator::Add));
    }

    #[test]
    fn test_rpn_unary() {
        let mut parser = Parser::new("3 + -4");
        let postfix = parser.postfix().unwrap();
        let expected = vec![
            Token::Number(3.0),
            Token::Number(4.0),
            Token::UnaryOperator(UnaryOperator::Sub),
            Token::Operator(Operator::Add),
        ];
        assert_eq!(expected, postfix);
    }

    #[test]
    fn test_compute() {
        let mut parser = Parser::new("3 + 4 * (2 - 1)");
        let result = parser.compute().unwrap();
        assert_eq!(result, 7.0);
    }

    #[test]
    fn test_unary() {
        let mut parser = Parser::new("3 - -3");
        let result = parser.compute().unwrap();
        assert_eq!(result, 6.0);
    }
}
