#![allow(dead_code)]
use std::iter::Peekable;
use std::str::Chars;
use std::error;
use std::fmt;
use std::collections::HashMap;
use std::f64::consts;

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),

    RightParen,
    LeftParen,

    Operator(Operator),
    EOF
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position(usize, usize);

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken(Position),
    UnbalancedParens,
    InvalidExpression
}

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            ParseError::UnexpectedToken(pos) => write!(f, "unexpected token at {}:{}", pos.0 + 1, pos.1),
            ParseError::UnbalancedParens => write!(f, "unbalanced parentheses found"),
            ParseError::InvalidExpression => write!(f, "an invalid expression was provided")
        };
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    constants: HashMap<&'static str, f64>
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            chars: src.chars().peekable(),
            line: 0,
            column: 0,
            constants: map!(
                "pi" => consts::PI
            )
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.next();
        if let Some('\n') = c {
            self.column = 0;
            self.line += 1;
        } else {
            self.column += 1;
        }
        return c;
    }

    pub fn lex(&mut self) -> Result<Token, ParseError> {
        Ok(match self.next() {
            Some(c) => match c {
                ' ' | '\t' | '\n' => self.lex()?,
                '0'..='9' => {
                    let start_pos = self.position();
                    let mut mantissa = c.to_string();
                    let mut exponent = String::new();
                    let mut in_exp = false;
                    let mut one_dot = false;
                    // numbers are f64s by default
                    while let Some(c) = self.chars.peek() {
                        match c {
                            '_' => {
                                self.next().unwrap();
                                if self.chars.peek() == Some(&'_') {
                                    self.next().unwrap();
                                    return Err(ParseError::UnexpectedToken(self.position()));
                                }
                                continue;
                            }
                            '0'..='9' => {
                                if in_exp {
                                    exponent.push(self.next().unwrap());
                                } else {
                                    mantissa.push(self.next().unwrap());
                                }
                            }
                            'e' if !in_exp => {
                                self.next().unwrap();
                                in_exp = true;
                                match self.chars.peek() {
                                    Some('-') => {
                                        self.next().unwrap();
                                        exponent.push('-');
                                    }
                                    Some('+') => {
                                        self.next().unwrap();
                                    }
                                    _ => {}
                                }
                            }

                            '.' if !in_exp => {
                                if !one_dot {
                                    one_dot = true;
                                    mantissa.push(self.next().unwrap());
                                    if self.chars.peek() == Some(&'_') {
                                        return Err(ParseError::UnexpectedToken(self.position()));
                                    }
                                } else {
                                    break;
                                }
                            }
                            _ => break,
                         }
                    }
                    match mantissa.parse::<f64>() {
                       Ok(n) => {
                           if in_exp {
                               match exponent.parse::<i32>() {
                                   Ok(e) => Token::Number(n * (10f64.powi(e) as f64)),
                                   Err(_) => return Err(ParseError::UnexpectedToken(start_pos))
                               }
                           } else {
                               Token::Number(n)
                           }
                       }
                        Err(_) => return Err(ParseError::UnexpectedToken(start_pos))
                    }

                }
                'a'...'z' | 'A'...'Z' => {
                    let mut ident = c.to_string();
                    let start_pos = self.position();
                    while let Some(c) = self.chars.peek() {
                        match c {
                            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                                ident.push(self.next().unwrap())
                            }
                            _ => break,
                        }
                    }
                    match self.constants.contains_key(ident.as_str()) {
                        true => Token::Number(self.constants.get(ident.as_str()).unwrap().to_owned()),
                        false => return Err(ParseError::UnexpectedToken(start_pos))
                    }
                }
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '*' => match self.chars.peek() {
                    Some('*') => {
                        self.next();
                        Token::Operator(Operator::Pow)
                    }
                    _ => Token::Operator(Operator::Mul)
                },
                '/' => Token::Operator(Operator::Div),
                '^' => Token::Operator(Operator::Pow),
                '+' => Token::Operator(Operator::Add),
                '-' => Token::Operator(Operator::Sub),
                _ => return Err(ParseError::UnexpectedToken(self.position()))
            },
            None => Token::EOF
        })
    }

    fn position(&self) -> Position {
        Position(self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        const INPUT: &str = "22_00 + 3e2";
        let mut lexer = Lexer::new(INPUT);
        assert_eq!(Token::Number(2200f64), lexer.lex().unwrap());
        assert_eq!(Token::Operator(Operator::Add), lexer.lex().unwrap());
        assert_eq!(Token::Number(300f64), lexer.lex().unwrap());
        assert_eq!(Token::EOF, lexer.lex().unwrap());
    }
}