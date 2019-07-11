#![allow(dead_code)]
use std::collections::HashMap;
use std::f64::consts;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Sub,
    Factorial,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),

    RightParen,
    LeftParen,

    Operator(Operator),
    UnaryOperator(UnaryOperator),
    EOF,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position(usize, usize);

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    UnexpectedToken(Position),
    UnbalancedParens,
    InvalidExpression,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(pos) => {
                write!(f, "unexpected token at {}:{}", pos.0 + 1, pos.1)
            }
            ParseError::UnbalancedParens => write!(f, "unbalanced parentheses found"),
            ParseError::InvalidExpression => write!(f, "an invalid expression was provided"),
        }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    constants: HashMap<&'static str, f64>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(Token::EOF) => None,
            Err(e) => Some(Err(e)),
            Ok(token) => Some(Ok(token)),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            chars: src.chars().peekable(),
            line: 0,
            column: 0,
            constants: map!(
                "pi" => consts::PI
            ),
        }
    }

    fn consume(&mut self) -> Option<char> {
        let c = self.chars.next();
        if let Some('\n') = c {
            self.column = 0;
            self.line += 1;
        } else {
            self.column += 1;
        }
        c
    }

    pub fn lex(&mut self) -> Result<Token, ParseError> {
        Ok(match self.consume() {
            Some(c) => match c {
                ' ' | '\t' | '\n' => self.lex()?,
                '0'...'9' => Token::Number(self.consume_number(c)?),
                'a'...'z' | 'A'...'Z' => {
                    let mut ident = c.to_string();
                    let start_pos = self.current_position();
                    while let Some(c) = self.chars.peek() {
                        match c {
                            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                                ident.push(self.consume().unwrap())
                            }
                            _ => break,
                        }
                    }
                    if self.constants.contains_key(ident.as_str()) {
                        Token::Number(self.constants.get(ident.as_str()).unwrap().to_owned())
                    } else {
                        return Err(ParseError::UnexpectedToken(start_pos));
                    }
                }
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '*' => match self.chars.peek() {
                    Some('*') => {
                        self.consume();
                        Token::Operator(Operator::Pow)
                    }
                    _ => Token::Operator(Operator::Mul),
                },
                '/' => Token::Operator(Operator::Div),
                '^' => Token::Operator(Operator::Pow),
                '+' => Token::Operator(Operator::Add),
                '-' => {
                    if self.peek_number() {
                        return Ok(Token::UnaryOperator(UnaryOperator::Sub));
                    }
                    return Ok(Token::Operator(Operator::Sub));
                }
                '!' => Token::UnaryOperator(UnaryOperator::Factorial),
                _ => return Err(ParseError::UnexpectedToken(self.current_position())),
            },
            None => Token::EOF,
        })
    }

    fn peek_number(&mut self) -> bool {
        if let Some(&c) = self.chars.peek() {
            match c {
                '1'...'9' => return true,
                _ => return false,
            };
        }
        false
    }

    fn consume_number(&mut self, c: char) -> Result<f64, ParseError> {
        // leading 0 indicates different base
        // 0x => hexadecimal, 0b => binary, 0o => octal
        if c == '0' {
            let radix = match self.chars.peek() {
                Some('x') => Some(16),
                Some('o') => Some(8),
                Some('b') => Some(2),
                _ => None,
            };
            if let Some(rdx) = radix {
                self.consume();
                let mut num = String::new();
                let start_pos = self.current_position();
                while let Some(c) = self.chars.peek() {
                    match c {
                        '0' | '1' => num.push(self.consume().unwrap()),
                        '2'...'7' if rdx > 2 => num.push(self.consume().unwrap()),
                        '8' | '9' | 'a'...'f' | 'A'...'F' if rdx > 8 => {
                            num.push(self.consume().unwrap());
                        }
                        _ => break,
                    }
                }
                let n = match u64::from_str_radix(&num, rdx) {
                    Ok(n) => n,
                    Err(_) => return Err(ParseError::UnexpectedToken(start_pos)),
                };
                return Ok(n as f64);
            } else {
                return Ok(0.0);
            }
        }

        let start_pos = self.current_position();
        let mut mantissa = c.to_string();
        let mut exponent = String::new();
        let mut in_exp = false;
        let mut one_dot = false;
        // numbers are f64s by default
        while let Some(c) = self.chars.peek() {
            match c {
                '_' => {
                    self.consume().unwrap();
                    if self.chars.peek() == Some(&'_') {
                        self.consume().unwrap();
                        return Err(ParseError::UnexpectedToken(self.current_position()));
                    }
                    continue;
                }
                '0'...'9' => {
                    if in_exp {
                        exponent.push(self.consume().unwrap());
                    } else {
                        mantissa.push(self.consume().unwrap());
                    }
                }
                'e' if !in_exp => {
                    self.consume().unwrap();
                    in_exp = true;
                    match self.chars.peek() {
                        Some('-') => {
                            self.consume().unwrap();
                            exponent.push('-');
                        }
                        Some('+') => {
                            self.consume().unwrap();
                        }
                        _ => {}
                    }
                }

                '.' if !in_exp => {
                    if !one_dot {
                        one_dot = true;
                        mantissa.push(self.consume().unwrap());
                        if self.chars.peek() == Some(&'_') {
                            return Err(ParseError::UnexpectedToken(self.current_position()));
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
                        Ok(e) => Ok(n * (10f64.powi(e) as f64)),
                        Err(_) => Err(ParseError::UnexpectedToken(start_pos)),
                    }
                } else {
                    Ok(n)
                }
            }
            Err(_) => return Err(ParseError::UnexpectedToken(start_pos)),
        }
    }

    fn current_position(&self) -> Position {
        Position(self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        const INPUT: &str = "22e2 + 3_00";
        let lexer = Lexer::new(INPUT);
        let tokens = lexer.map(|r| r.unwrap()).collect::<Vec<Token>>();
        assert_eq!(
            tokens,
            vec![
                Token::Number(2200.0),
                Token::Operator(Operator::Add),
                Token::Number(300.0)
            ]
        )
    }

    #[test]
    fn test_unary() {
        const INPUT: &str = "-3 + 2";
        let lexer = Lexer::new(INPUT);
        let tokens = lexer.map(|r| r.unwrap()).collect::<Vec<Token>>();
        assert_eq!(
            tokens,
            vec![
                Token::UnaryOperator(UnaryOperator::Sub),
                Token::Number(3.0),
                Token::Operator(Operator::Add),
                Token::Number(2.0)
            ]
        )
    }
}
