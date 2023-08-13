use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
struct RewindAndPeekable {
    input: String,
    pos: usize
}

impl RewindAndPeekable {

    fn new(input: impl AsRef<str>) -> Self {
        RewindAndPeekable {
            input: input.as_ref().to_string(),
            pos: 0
        }
    }
    fn peek(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        } else {
            return self.input.chars().nth(self.pos);
        }
    }

    fn next(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        } else {
            let result = self.input.chars().nth(self.pos);
            self.pos += 1;
            return result
        }
    }

    fn reset(&mut self, pos: usize) {
        assert!(pos < self.input.len());
        self.pos = pos;
    }

}

#[derive(Debug)]
struct Tokenizer {
    input: RewindAndPeekable,
    pos: usize,
    line: usize,
    col: usize,
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Literal(Vec<Char>),
    Digit(char),
    Identifier(String),
    ForwardSlash,
    BackSlash,
    Period,
    Asterisk,
    SingleQuote,
    DoubleQuote,
    LeftBracket,
    RightBracket,
    Hyphen,
    And,
    Not,
    LeftArrow,
    LeftAngleBracket,
    RightAngleBracket,
    Question,
    Plus,
    OpenParen,
    CloseParen,
    EndOfLine,
    EndOfFile,
    Spacing,
    Char(Char), // Contains a char which could be '\n', '\r', '\t',  '\[0-2][0-7][0-7]',
                        // '\[0-7][0-7]', or any character
}

#[derive(Debug, PartialEq)]
struct Char {
    ch: String
}

#[derive(Debug, PartialEq)]
struct Token {
    ttype: TokenType,
    line: usize,
    col: usize
}

#[macro_export]
macro_rules! token_arm { 
    ($self:ident, $var:path) => {
      {
           $self.consume_next();
           Some(Token {
               ttype: $var,
               line: $self.line,
               col: $self.col
           })
       }
    };
}

impl Tokenizer {
    fn new(input: &str) -> Self {
        Tokenizer{ input: RewindAndPeekable::new(input), pos: 0, line: 1, col: 0}
    }

    fn mark(&self) -> usize {
        self.pos
    }

    fn reset(&mut self, new_pos: usize)  {
        self.pos = new_pos;
        self.input.reset(new_pos);
    }

    fn peek_ch(&mut self) -> Option<char> {
        self.input.peek()
    }
    // credit: from Erik Grinaker's toydb sql parser
    // this is pretty cool, it uses peekable to ensure next token satifies
    // predicate, i.e. isdigit(). if it doesn't it will return early via ?
    fn next_if<F: Fn(char) -> bool>(&mut self, predicate: F) -> Option<char> {
        self.input.peek().filter(|c| predicate(*c))?;
        self.consume_next()
    }

    // keep grabbing next token as long as it's whitespace
    fn consume_whitespace(&mut self) {
       while let Some(c) = self.next_if(|c| c == ' ' || c == '\t' || c == '\n') { 
           match c {
               ' ' | '\t' => {
                self.pos += 1;
                self.col += 1;
               },
               '\n' => {
                   self.pos += 1;
                   self.col = 1;
                   self.line += 1;
               },
               _ => {} 
           }
       }
    }

    fn scan(&mut self) -> Option<Token> {
        self.consume_whitespace();
        match self.input.peek() {
            Some('\'') => self.scan_literal(),
            Some('[') => token_arm!(self, TokenType::LeftBracket),
            Some(']') => token_arm!(self, TokenType::RightBracket),
            Some('/') => token_arm!(self, TokenType::ForwardSlash),
            Some('\\') => token_arm!(self, TokenType::BackSlash),
            Some('.') => token_arm!(self, TokenType::Period),
            Some('*') => token_arm!(self, TokenType::Asterisk),
            Some('"') => token_arm!(self, TokenType::DoubleQuote),
            Some('-') => token_arm!(self, TokenType::Hyphen),
            Some('&') => token_arm!(self, TokenType::And),
            Some('!') => token_arm!(self, TokenType::Not),
            Some('<') => self.scan_leftangle(),
            Some('>') => token_arm!(self, TokenType::RightAngleBracket),
            Some('+') => token_arm!(self, TokenType::Plus),
            Some('?') => token_arm!(self, TokenType::Question),
            Some('(') => token_arm!(self, TokenType::OpenParen),
            Some(')') => token_arm!(self, TokenType::CloseParen),
            Some('\n') => token_arm!(self, TokenType::EndOfLine),
            Some(ch) if ch.is_alphabetic() => self.scan_identifier(),
            Some(ch) => {
                println!("unhandled ch: {}, line: {}, col: {}", ch, self.line, self.col);
                None
            },
            None => None
        }
    }

    fn consume_next(&mut self) -> Option<char> {
        let ch = self.input.peek();
        if let Some('\n') = ch {
            self.pos += 1;
            self.col = 1;
            self.line += 1;
        } else {
            self.pos += 1;
            self.col += 1
        }
        self.input.next()
    }

    fn scan_identifier(&mut self) -> Option<Token> {
        let mut ident = String::new();
        while let Some(ch) = self.next_if(|c| c.is_alphanumeric()) {
            ident.push(ch)
        }
        Some(Token {
            ttype: TokenType::Identifier(ident),
            line: self.line,
            col: self.col
        })
    }

    fn scan_leftangle(&mut self) -> Option<Token> {
        self.consume_next();
        let c = self.next_if(|c| c == '-');
        match c {
            Some('-') => {
                Some(Token {
                    ttype: TokenType::LeftArrow,
                    line: self.line,
                    col: self.col})
            },
            Some(_) => {
                // This should never happen
                unreachable!("scan_leftangle got unexpected char");
            },
            None => {
                Some(Token {
                    ttype: TokenType::LeftAngleBracket,
                    line: self.line,
                    col: self.col})
            }
        }
    }
    fn scan_literal(&mut self) -> Option<Token> {
        // Eat beginning single quote
        self.consume_next();
        let mut chars = Vec::new();
        loop {
            let c = self.input.peek();
            println!("c: {:?}", c);
            if let Some('\'') = c {
                println!("finished literals");
                self.consume_next();
                break;
            } else {
                println!("current ch: {:?}", c);
                match c { 
                    Some('\\') => {
                        println!("scan_literal found \\");
                        let escaped_char = self.scan_escaped_char();
                        println!("escaped_char: {:?}", escaped_char);
                        if let Some(escaped_char) = escaped_char {
                            chars.push(escaped_char)
                        }
                    },
                    Some(c) => {
                        chars.push(Char { 
                            ch: format!("{}", c)
                        });
                        self.consume_next();
                    }
                    None => {
                        break;
                    },
                }
            }
        }
        Some(Token {
            ttype: TokenType::Literal(chars),
            line: self.line,
            col: self.col
        })
    }

    fn scan_escaped_char(&mut self) -> Option<Char> {
        // Eat the '\'
        println!("in scan_escpaed_char");
        self.consume_next();
        let c = self.next_if(|c| ('0'..='7').contains(&c));
        let ch = if let Some(c) = c {
            let c2 = self.next_if(|c| ('0'..='7').contains(&c));
            if let Some(c2) = c2 {
                let c3 = self.next_if(|c| ('0'..='7').contains(&c));
                if let Some(c3) = c3 {
                    if ('0'..='2').contains(&c) {
                        Some(Char { ch: (format!("\\{}{}{}", c,c2,c3)) })
                    } else {
                        None
                    }
                } else {
                    Some(Char { ch: format!("\\{}{}", c ,c2) })
                }
            } else {
                None
            }
        } else {
            self.next_if(|c| ['n', 'r', 't'].contains(&c))
                .map(|c|
                     Char { ch: format!("\\{}", c) })
        };
        ch
    }


}
pub mod test { 
    #[allow(unused_imports)]
    use super::{Tokenizer, Token, TokenType};

    #[test]
    pub fn test_consumes_whitespace() {
        let mut tokenizer = Tokenizer::new("    a   b");
        let ch1 = tokenizer.scan().map(|t| t.ttype);
        let ch2 = tokenizer.scan().map(|t| t.ttype);
        assert_eq!(ch1, Some(TokenType::Identifier("a".to_string())));
        assert_eq!(ch2, Some(TokenType::Identifier("b".to_string())));
    }

    #[test]
    pub fn test_scan_nonalpha() {
        let mut tokenizer = Tokenizer::new("[]");
        let tok = tokenizer.scan();
        if let Some(Token{ ttype: TokenType::LeftBracket, ..}) = tok {
        } else {
            panic!("Expected left bracket");
        }

    }

    #[test]
    pub fn test_scan_identifier() {
        let mut tokenizer = Tokenizer::new("test <- nonterminal");
        let tok = tokenizer.scan();
        if let Some(Token{ ttype: TokenType::Identifier(_), ..}) = tok {
        } else {
            panic!("Expected identifier");
        }
    }

    
    #[test]
    pub fn test_scan_literal() {
        let mut tokenizer = Tokenizer::new(r#"'a\n\t'"#);
        let tok = tokenizer.scan_literal();
        println!("tok: {:?}", tok);
        if let Some(Token{ ttype: TokenType::Literal(chars), ..}) = tok {
            assert_eq!(chars.len(), 3, "Expected 3 chars");
        } else {
            panic!("No Char parsed");
        }
    }
    
    #[test]
    pub fn test_scan_and_rewind_then_scan() {
        let mut tokenizer = Tokenizer::new(r#"a b c"#);
        let tok = tokenizer.scan();
        let ch = tok.map(|t| t.ttype);
        assert_eq!(ch, Some(TokenType::Identifier("a".to_string())));
        let tok = tokenizer.scan();
        let ch = tok.map(|t| t.ttype);
        assert_eq!(ch, Some(TokenType::Identifier("b".to_string())));
        let tok = tokenizer.scan();
        let ch = tok.map(|t| t.ttype);
        assert_eq!(ch, Some(TokenType::Identifier("c".to_string())));
        tokenizer.reset(2);
        let tok = tokenizer.scan();
        let ch = tok.map(|t| t.ttype);
        assert_eq!(ch, Some(TokenType::Identifier("b".to_string())));
    }
    
}
