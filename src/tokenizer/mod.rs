mod errors;

pub use errors::*;
use std::iter::Peekable;
use std::str::Chars;

use crate::{Keyword, Token, TokenType};

/// SQL statements are split into tokens by the
/// [`Tokenizer`] component of SQLite.
///
/// When processing SQL statements, SQLite (as does every other SQL database
/// engine) breaks the SQL statement up into [`token`](crate::Token) which are then forwarded
/// to the parser component.
///
/// This document specifies requirements that precisely define the operation
/// of the SQLite tokenizer.
///
/// ### Character classes
///
///
/// SQL statements are composed of unicode characters. Specific individual
/// characters many be described using a notation consisting of the character
/// "u" followed by four hexadecimal digits.
///
/// For example, the lower-case letter `"a"` can be expressed as `"u0061"`
/// and the dollar sign can be expressed as "u0024". For notational convenience,
/// the following character classes are defined:
///
/// - WHITESPACE
///   One of these five characters: u0009, u000a, u000c, u000d, or u0020
///
/// - ALPHABETIC
///   Any of the characters in the range u0041 through u005a (letters "A"
///   through "Z") or in the range u0061 through u007a (letters "a" through "z")
///   or the character u005f ("_") or any other character larger than u007f.
///
/// - NUMERIC
///   Any of the characters in the range u0030 through u0039 (digits "0"
///   through "9")
///
/// - ALPHANUMERIC
///   Any character which is either ALPHABETIC or NUMERIC
///
/// - HEXADECIMAL
///   Any NUMERIC character or a characters in the range u0041 through u0046
///   ("A" through "F") or in the range u0061 through u0066 ("a" through "f")
///
/// - SPECIAL
///   Any character that is not WHITESPACE, ALPHABETIC, nor NUMERIC
///
/// ### Token requirements
///
/// - Processing is left-to-right. This seems obvious, but it needs to be
///   explicitly stated.
///
/// - H41010: SQLite shall divide input SQL text into tokens working from
///   left to right. The standard practice in SQL, as with most context-free
///   grammar based programming languages, is to resolve ambiguities in
///   tokenizing by selecting the option that results in the longest tokens.
///
/// - H41020: At each step in the SQL tokenization process, SQLite shall
///   extract the longest possible token from the remaining input text.
///   
/// - The tokenizer recognizes tokens one by one and passes them on to the
///   parser. Except whitespace is ignored. The only use for whitespace is as a
///   separator between tokens.
///
/// - H41030: The tokenizer shall pass each non-WHITESPACE token seen on to
///   the parser in the order in which the tokens are seen. The tokenizer
///   appends a semicolon to the end of input if necessary. This ensures that
///   every SQL statement is terminated by a semicolon.
///
/// - H41040: When the tokenizer reaches the end of input where the last token
///   sent to the parser was not a SEMI token, it shall send a SEMI token to
///   the parser. An unrecognized token generates an immediate error and aborts
///   the parse.
///
/// - H41050: When the tokenizer encounters text that is not a valid token, it
///   shall cause an error to be returned to the application.
///
///
/// #### Whitespace tokens
///
/// Whitespace has the usual definition.
///
/// - H41100: SQLite shall recognize a sequence of one or more WHITESPACE
///   characters as a WHITESPACE token. An SQL comment is "--" through the end
///   of line and is understood as whitespace.
///
/// - H41110: SQLite shall recognize as a WHITESPACE token the two-character
///   sequence "--" (u002d, u002d) followed by any sequence of non-zero
///   characters up through and including the first u000a character or until
///   end of input. A C-style comment "/*...*/" is also recognized as
///   white-space.
///
/// - H41120: SQLite shall recognize as a WHITESPACE token the two-character
///   sequence "/*" (u002f, u002a) followed by any sequence of zero or more
///   non-zero characters through with the first "*/" (u002a, u002f) sequence
///   or until end of input.
///
///
/// #### Identifier tokens
///
/// Identifiers follow the usual rules with the exception that SQLite allows
/// the dollar-sign symbol in the interior of an identifier. The dollar-sign
/// is for compatibility with Microsoft SQL-Server and is not part of the SQL
/// standard.
///
/// - H41130: SQLite shall recognize as an ID token any sequence of characters
///   that begins with an ALPHABETIC character and continue with zero or more
///   ALPHANUMERIC characters and/or "$" (u0024) characters and which is not a
///   keyword token. Identifiers can be arbitrary character strings within
///   square brackets. This feature is also for compatibility with Microsoft
///   SQL-Server and not a part of the SQL standard.
///
/// - H41140: SQLite shall recognize as an ID token any sequence of non-zero
///   characters that begins with "[" (u005b) and continuing through the
///   first "]" (u005d) character. The standard way of quoting SQL identifiers
///   is to use double-quotes.
///
/// - H41150: SQLite shall recognize as an ID token any sequence of characters
///   that begins with a double-quote (u0022), is followed by zero or more
///   non-zero characters and/or pairs of double-quotes (u0022) and terminates
///   with a double-quote (u0022) that is not part of a pair. MySQL allows
///   identifiers to be quoted using the grave accent character. SQLite
///   supports this for interoperability.
///
/// - H41160: SQLite shall recognize as an ID token any sequence of characters
///   that begins with a grave accent (u0060), is followed by zero or more
///   non-zero characters and/or pairs ofgrave accents (u0060) and terminates
///   with a grave accent (u0022) that is not part of a pair.
///
///
/// ### Literals
///
/// This is the usual definition of string literals for SQL. SQL uses the
/// classic Pascal string literal format.
///
/// - H41200: SQLite shall recognize as a STRING token a sequence of
///   characters that begins with a single-quote (u0027), is followed by zero
///   or more non-zero characters and/or pairs of single-quotes (u0027) and
///   terminates with a single-quote (u0027) that is not part of a pair.
///   Blob literals are similar to string literals except that they begin with
///   a single "X" character and contain hexadecimal data.
///
/// - H41210: SQLite shall recognize as a BLOB token an upper or lower-case
///   "X" (u0058 or u0078) followed by a single-quote (u0027) followed by a
///   number of HEXADECIMAL character that is a multiple of two and terminated
///   by a single-quote (u0027). Integer literals are a string of digits. The
///   plus or minus sign that might optionally preceed an integer is not part
///   of the integer token.
///
/// - H41220: SQLite shall recognize as an INTEGER token any squence of one or
///   more NUMERIC characters. An "exponentiation suffix" is defined to be an
///   upper or lower case "E" (u0045 or u0065) followed by one or more NUMERIC
///   characters. The "E" and the NUMERIC characters may optionally be separated
///   by a plus-sign (u002b) or a minus-sign (u002d). An exponentiation suffix
///   is part of the definition of a FLOAT token:
///
/// - H41230: SQLite shall recognize as a FLOAT token a sequence of one or
///   more NUMERIC characters together with zero or one period (u002e) and
///   followed by an exponentiation suffix.
///
/// - H41240: SQLite shall recognize as a FLOAT token a sequence of one or
///   more NUMERIC characters that includes exactly one period (u002e)
///   character.
///
///
/// #### Variables
///
/// Variables are used as placeholders in SQL statements for constant values
/// that are to be bound at start-time.
///
/// - H40310: SQLite shall recognize as a VARIABLE token the a question-mark
///   (u003f) followed by zero or more NUMERIC characters. A "parameter name"
///   is defined to be a sequence of one or more characters that consists of
///   ALPHANUMERIC characters and/or dollar-signs (u0025) intermixed with pairs
///   of colons (u003a) and optionally followed by any sequence of non-zero,
///   non-WHITESPACE characters enclosed in parentheses (u0028 and u0029).
///
/// - H40320: SQLite shall recognize as a VARIABLE token one of the characters
///   at-sign (u0040), dollar-sign (u0024), or colon (u003a) followed by a
///   parameter name.
///
/// - H40330: SQLite shall recognize as a VARIABLE token the shape-sign
///   (u0023) followed by a parameter name that does not begin with a NUMERIC
///   character.
///
/// The REGISTER token is a special token used internally. It does not appear
/// as part of the published user interface. Hence, the following is a
/// low-level requirement:
/// - L42040: SQLite shall recognize as a REGISTER token a sharp-sign (u0023)
///   followed by one or more NUMERIC characters.
#[derive(Debug)]
pub struct Tokenizer<'input> {
    raw_content: &'input str,
    chars: Peekable<Chars<'input>>,
    current_pos: usize,
    last_semi_token_pos: Option<usize>,
}

impl<'input> Tokenizer<'input> {
    /// Consumes the next character, updates `current_pos`, and returns the character.
    fn next_char(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            self.current_pos += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    /// Peeks at the next character without consuming it.
    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Skips over whitespace characters, updating `current_pos`.
    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.peek_char() {
            if Self::is_whitespace(&ch) {
                // Consume the whitespace character
                self.next_char();
            } else {
                break;
            }
        }
    }

    /// Checks if a character is a valid start character for an identifier.
    fn is_id_start_char(c: &char) -> bool {
        c.is_alphabetic() || *c == '_'
    }

    /// Checks if a character is a valid part of an identifier.
    fn is_id_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '$'
    }

    /// Checks if a character is a valid start character for a parameter name.
    fn is_param_name_start_char(c: char) -> bool {
        c.is_alphabetic() || c == '$'
    }

    /// Checks if a character is valid within a parameter name.
    fn is_param_name_char(c: char) -> bool {
        c.is_alphanumeric() || c == '$' || c == ':'
    }

    /// There are 5 symbols by the specification considered as whitespace, which
    /// maps to the ASCII whitespaces. Unicode whitespace from Rust's
    /// `char::is_whitespace()` considers way more charaters as whitespaces, so
    /// we're using only ASCII whitespaces
    fn is_whitespace(char: &char) -> bool {
        char.is_ascii_whitespace()
    }

    /// Parses a Blob literal from the steam of characters
    fn parse_blob_literal(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        // Collect hexadecimal characters
        while let Some(&c) = self.peek_char() {
            if c == '\'' {
                break; // End of BLOB
            }
            if c.is_ascii_hexdigit() {
                self.next_char();
            } else {
                // Invalid character in BLOB
                return Some(Err(TokenizerError::MalformedBlobLiteral(
                    &self.raw_content[start_pos..],
                    self.current_pos,
                )));
            }
        }

        // Ensure closing single quote is present
        if self.peek_char() == Some(&'\'') {
            self.next_char(); // Consume closing single quote
        } else {
            // Unterminated BLOB literal
            return Some(Err(TokenizerError::UnterminatedLiteral(
                &self.raw_content[start_pos..],
            )));
        }

        let hex_digits = &self.raw_content[start_pos..self.current_pos];

        // Check if the number of hex digits is even (minus 3 for x and two \' symbols)
        if (hex_digits.len() - 3) % 2 != 0 {
            return Some(Err(TokenizerError::MalformedBlobLiteral(
                &self.raw_content[start_pos..],
                self.current_pos,
            )));
        }

        Some(Ok(Token {
            token_type: TokenType::Blob(hex_digits),
            position: start_pos,
        }))
    }

    /// Parses an Id, Keyword or Blob literals from the steam of characters
    fn parse_literal(
        &mut self,
        start_pos: usize,
        current_char: char,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        //  Check if BLOB token started
        if (current_char == 'X' || current_char == 'x') && (self.peek_char() == Some(&'\'')) {
            self.next_char(); // Consume opening single quote '
            return self.parse_blob_literal(start_pos);
        }

        // Identifier or keyword
        while let Some(&ch) = self.peek_char() {
            if Self::is_id_char(ch) {
                self.next_char();
            } else {
                break;
            }
        }

        let text = &self.raw_content[start_pos..self.current_pos];
        // Check if it's a keyword
        if let Ok(keyword) = Keyword::try_from(text) {
            Some(Ok(Token {
                token_type: TokenType::Keyword(keyword),
                position: start_pos,
            }))
        } else if text.to_lowercase() == "true" {
            return Some(Ok(Token {
                token_type: TokenType::True,
                position: start_pos,
            }));
        } else if text.to_lowercase() == "false" {
            return Some(Ok(Token {
                token_type: TokenType::False,
                position: start_pos,
            }));
        } else {
            // It's an identifier
            return Some(Ok(Token {
                token_type: TokenType::Id(text),
                position: start_pos,
            }));
        }
    }

    /// Parses an Id placed between square brackets [id]  
    fn parse_identifier_between_square_brackets(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        while let Some(c) = self.next_char() {
            if c == ']' {
                // Single ] terminator symbol found
                let text = &self.raw_content[start_pos..self.current_pos];
                if text.len() == 2 {
                    // 2 is open and close square brackets only (no content inside)
                    return Some(Err(TokenizerError::EmptyId));
                }
                return Some(Ok(Token {
                    token_type: TokenType::Id(text),
                    position: start_pos,
                }));
            } else {
                // Regular character, continue
            }
        }

        // Unexpected EOF, an identifier is not closed properly
        Some(Err(TokenizerError::UnexpectedEOF))
    }

    /// Parses an Id placed between grave_accent symbols `id` or square brackets [id]  
    fn parse_identifier_between_grave_accent(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        while let Some(c) = self.next_char() {
            if c == '\u{0060}' {
                // Grave accent found
                if let Some(&next_c) = self.peek_char() {
                    if next_c == '\u{0060}' {
                        // Pair of grave accents, consume next ` symbol
                        self.next_char();
                        continue;
                    }
                }
                // Single grave accent, terminator found

                let text = &self.raw_content[start_pos..self.current_pos];
                return Some(Ok(Token {
                    token_type: TokenType::Id(text),
                    position: start_pos,
                }));
            } else {
                // Regular character, continue
            }
        }

        // Unexpected EOF, no closing grave_accent found
        Some(Err(TokenizerError::UnexpectedEOF))
    }

    /// Parses a numberic literal: Integer or Float, including scientific notation
    fn parse_hex_numberic_literal(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        while let Some(&next_ch) = self.peek_char() {
            if next_ch.is_ascii_hexdigit() {
                self.next_char(); // consume hex digit
            } else {
                break;
            }
        }

        // If next character is an EOF, return the Literal
        if self.peek_char().is_none() {
            let token = Token {
                token_type: TokenType::Integer(&self.raw_content[start_pos..self.current_pos]),
                position: start_pos,
            };
            return Some(Ok(token));
        }

        // If next character is a whitespace, return the Literal
        if let Some(&next_ch) = self.peek_char() {
            if Self::is_whitespace(&next_ch) {
                // Invalid number: contains invalid character
                let token = Token {
                    token_type: TokenType::Integer(&self.raw_content[start_pos..self.current_pos]),
                    position: start_pos,
                };
                return Some(Ok(token));
            }
        }
        Some(Err(TokenizerError::BadNumber))
    }

    /// Parses a numberic literal: Integer or Float, including scientific notation
    fn parse_numberic_literal(
        &mut self,
        start_pos: usize,
        current_char: char,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        let mut has_dot = false;
        let mut has_exponent = false;
        let mut is_valid = true;
        // To track the number of consecutive '_' symbols
        let mut last_underscore_symbol_position: Option<usize> = None;

        // check for the hex literal 0x[value]
        if current_char == '0' {
            if let Some(&next_ch) = self.peek_char() {
                if next_ch == 'x' {
                    self.next_char(); // consume 'x'
                    return self.parse_hex_numberic_literal(start_pos);
                }
            }
        }

        while let Some(&next_ch) = self.peek_char() {
            if next_ch.is_ascii_digit() {
                self.next_char();
            } else if next_ch == '.' {
                // already has dot in a previous characters
                if has_dot {
                    return Some(Err(TokenizerError::BadNumber));
                } else {
                    has_dot = true;
                    self.next_char();
                    // Check for digits after '.'
                    if let Some(&after_dot_ch) = self.peek_char() {
                        if !after_dot_ch.is_ascii_digit() {
                            is_valid = false;
                            break;
                        }
                    } else {
                        is_valid = false;
                        break;
                    }
                }
            } else if (next_ch == 'e' || next_ch == 'E') && !has_exponent {
                has_exponent = true;
                self.next_char();
                // Exponent can be followed by '+' or '-' or digits
                if let Some(&exp_ch) = self.peek_char() {
                    if exp_ch == '+' || exp_ch == '-' {
                        self.next_char();
                    }
                }
                // There should be digits after exponent
                if let Some(&digit_ch) = self.peek_char() {
                    if !digit_ch.is_ascii_digit() {
                        is_valid = false;
                        break;
                    }
                } else {
                    is_valid = false;
                    break;
                }
            } else if next_ch == '_' {
                // check if there are couple of '_' symbols in a row
                if let Some(position) = last_underscore_symbol_position {
                    if self.current_pos - position == 1 {
                        // there are two _ symbols in a row
                        return Some(Err(TokenizerError::BadNumber));
                    }
                }
                // consume and ignore '_' character as it's only for number formatting
                last_underscore_symbol_position = Some(self.current_pos);
                self.next_char();
                continue;
            } else {
                break;
            }
        }

        // Check for invalid characters in the number
        if is_valid {
            // Ensure that the next character is not an identifier character
            if let Some(&next_ch) = self.peek_char() {
                if Self::is_id_char(next_ch) {
                    // Invalid number: contains invalid character
                    is_valid = false;
                }
            }
        }

        if !is_valid {
            return Some(Err(TokenizerError::BadNumber));
        }

        // check if number ends with '_' symbol
        if let Some(position) = last_underscore_symbol_position {
            if self.current_pos - position == 1 {
                return Some(Err(TokenizerError::BadNumber));
            }
        }

        let text = &self.raw_content[start_pos..self.current_pos];
        let token_type = if has_dot || has_exponent {
            TokenType::Float(text)
        } else {
            TokenType::Integer(text)
        };

        Some(Ok(Token {
            token_type,
            position: start_pos,
        }))
    }

    /// Parses a string literal within single and double quotes
    fn parse_string_literal(
        &mut self,
        start_pos: usize,
        quote_char: char,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        loop {
            match self.next_char() {
                Some(ch) => {
                    if ch == quote_char {
                        match self.peek_char() {
                            Some(next_char) if *next_char == quote_char => {
                                // second quote (quote escaping)
                                self.next_char(); // Consume second quote symbol
                                continue;
                            }
                            // Closing quote found
                            _ => break,
                        }
                    }
                }
                None => {
                    // EOF reached without closing quote
                    return Some(Err(TokenizerError::UnterminatedLiteral(
                        &self.raw_content[start_pos..],
                    )));
                }
            }
        }

        let text = &self.raw_content[start_pos..self.current_pos];
        if quote_char == '\'' {
            Some(Ok(Token {
                token_type: TokenType::String(text),
                position: start_pos,
            }))
        } else {
            Some(Ok(Token {
                token_type: TokenType::Id(text),
                position: start_pos,
            }))
        }
    }

    /// Parses a variable placeholder as a single token one of the characters
    /// (@)at-sign, ($)dollar-sign, or (:) colon followed by a parameter name.
    fn parse_variable_placeholder(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        if let Some(&next_ch) = self.peek_char() {
            if !Self::is_param_name_start_char(next_ch) {
                // Invalid parameter name
                return Some(Err(TokenizerError::BadVariableName));
            }
        } else {
            // Unexpected EOF
            return Some(Err(TokenizerError::UnexpectedEOF));
        }

        while let Some(&next_ch) = self.peek_char() {
            if Self::is_param_name_char(next_ch) {
                self.next_char();
            } else if next_ch == '(' {
                // Consume the '(' and characters until ')'
                self.next_char(); // Consume '('
                while let Some(ch) = self.next_char() {
                    if ch == ')' {
                        break;
                    } else if ch == '\0' || ch.is_whitespace() {
                        return Some(Err(TokenizerError::BadVariableName));
                    }
                }
            } else {
                break;
            }
        }

        let text = &self.raw_content[start_pos..self.current_pos];
        Some(Ok(Token {
            token_type: TokenType::Variable(text),
            position: start_pos,
        }))
    }

    /// Parses a (#)sharp-sign variable placeholder as a single token one of the characters
    fn parse_variable_placeholder_sharp_sign(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        if let Some(&next_ch) = self.peek_char() {
            if !Self::is_param_name_start_char(next_ch) {
                // Invalid parameter name
                Some(Err(TokenizerError::BadVariableName))
            } else {
                // Variable token
                while let Some(&next_ch) = self.peek_char() {
                    if Self::is_param_name_char(next_ch) {
                        self.next_char();
                    } else if next_ch == '(' {
                        // Consume the '(' and characters until ')'
                        self.next_char(); // Consume '('
                        while let Some(ch) = self.next_char() {
                            if ch == ')' {
                                break;
                            } else if ch == '\0' || ch.is_whitespace() {
                                // Invalid character in parentheses
                                return Some(Err(TokenizerError::BadVariableName));
                            }
                        }
                    } else {
                        break;
                    }
                }
                let text = &self.raw_content[start_pos..self.current_pos];
                Some(Ok(Token {
                    token_type: TokenType::Variable(text),
                    position: start_pos,
                }))
            }
        } else {
            // Unexpected end of input after '#'
            Some(Err(TokenizerError::UnexpectedEOF))
        }
    }

    /// Parses a (?)question mark sign variable placeholder as a single token one of the characters
    fn parse_variable_placeholder_question_mark_sign(
        &mut self,
        start_pos: usize,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        while let Some(&next_ch) = self.peek_char() {
            if next_ch.is_ascii_digit() {
                self.next_char();
            } else {
                break;
            }
        }

        // After consuming digits, check for invalid characters following the variable name
        if let Some(&next_ch) = self.peek_char() {
            // The next symbols must be either WHITESPACE or ) or , or ;
            // all other symbols means wrong variable name
            if !Self::is_whitespace(&next_ch) && next_ch != ')' && next_ch != ',' && next_ch != ';'
            {
                return Some(Err(TokenizerError::BadVariableName));
            }
        }

        let text = &self.raw_content[start_pos..self.current_pos];
        Some(Ok(Token {
            token_type: TokenType::Variable(text),
            position: start_pos,
        }))
    }

    fn parse_operators(
        &mut self,
        start_pos: usize,
        current_char: char,
    ) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        // Operators and punctuation
        let token_type = match current_char {
            '+' => TokenType::Plus,
            '*' => TokenType::Star,
            '%' => TokenType::Remainder,
            '.' => TokenType::Dot,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            ';' => {
                self.last_semi_token_pos = Some(start_pos);
                TokenType::Semi
            }
            ',' => TokenType::Comma,
            '&' => TokenType::BitAnd,
            '~' => TokenType::BitNot,
            '-' => {
                // Check for '--' (single line comment)
                if let Some(&next_ch) = self.peek_char() {
                    if next_ch == '-' {
                        // Consume the second '-'
                        self.next_char();
                        let start_pos = self.current_pos;
                        while let Some(ch) = self.next_char() {
                            if ch == '\n' {
                                break;
                            }
                        }
                        TokenType::SingleLineComment(&self.raw_content[start_pos..self.current_pos])
                    } else {
                        TokenType::Minus
                    }
                } else {
                    TokenType::Minus
                }
            }
            '/' => {
                // Check for '/*' (multi-line comment)
                if let Some(&next_ch) = self.peek_char() {
                    if next_ch == '*' {
                        self.next_char(); // Consume '*'
                        let start_pos = self.current_pos;
                        let mut terminated = false;
                        while let Some(ch) = self.next_char() {
                            // search for closing "*/"
                            if ch == '*' {
                                if let Some(&next_ch) = self.peek_char() {
                                    if next_ch == '/' {
                                        self.next_char(); // Consume '/'
                                        terminated = true;
                                        break;
                                    }
                                }
                            }
                        }
                        if !terminated {
                            return Some(Err(TokenizerError::UnterminatedCommentBlock));
                        }
                        // minus 2 characters to ignore */ symbols
                        TokenType::MultiLineComment(
                            &self.raw_content[start_pos..self.current_pos - 2],
                        )
                    } else {
                        TokenType::Slash
                    }
                } else {
                    TokenType::Slash
                }
            }
            '|' => {
                // Check for '||' (concatenation operator)
                if let Some(&next_ch) = self.peek_char() {
                    if next_ch == '|' {
                        // Consume the second '|'
                        self.next_char();
                        TokenType::Concat
                    } else {
                        TokenType::BitOr
                    }
                } else {
                    TokenType::BitOr
                }
            }
            '=' => {
                // Check for '==' (equality operator)
                if let Some(&next_ch) = self.peek_char() {
                    if next_ch == '=' {
                        // Consume the second '='
                        self.next_char();
                        TokenType::EqualsEquals
                    } else {
                        TokenType::Equals
                    }
                } else {
                    TokenType::Equals
                }
            }
            '<' => {
                // Could be '<', '<=', '<>', '<<'
                if let Some(&next_ch) = self.peek_char() {
                    match next_ch {
                        '=' => {
                            // Consume '='
                            self.next_char();
                            TokenType::LessEquals
                        }
                        '>' => {
                            // Consume '>'
                            self.next_char();
                            TokenType::NotEquals
                        }
                        '<' => {
                            // Consume '<'
                            self.next_char();
                            TokenType::LeftShift
                        }
                        _ => TokenType::LessThan,
                    }
                } else {
                    TokenType::LessThan
                }
            }
            '>' => {
                // Could be '>', '>=', '>>'
                if let Some(&next_ch) = self.peek_char() {
                    match next_ch {
                        '=' => {
                            // Consume '='
                            self.next_char();
                            TokenType::GreaterEquals
                        }
                        '>' => {
                            // Consume '>'
                            self.next_char();
                            TokenType::RightShift
                        }
                        _ => TokenType::GreaterThan,
                    }
                } else {
                    TokenType::GreaterThan
                }
            }
            '!' => {
                // Could be '!='
                if let Some(&next_ch) = self.peek_char() {
                    if next_ch == '=' {
                        // Consume '='
                        self.next_char();
                        TokenType::NotEquals
                    } else {
                        // Return an error for unrecognized token
                        return Some(Err(TokenizerError::UnrecognizedToken));
                    }
                } else {
                    // Unexpected end of input after '!'
                    return Some(Err(TokenizerError::UnexpectedEOF));
                }
            }
            '?' => {
                // H40310: SQLite shall recognize as a VARIABLE token the question-mark followed by zero or more NUMERIC characters.
                return self.parse_variable_placeholder_question_mark_sign(start_pos);
            }
            ':' | '@' | '$' => {
                // H40320: SQLite shall recognize as a VARIABLE token one of the characters at-sign,
                // dollar-sign, or colon followed by a parameter name.
                return self.parse_variable_placeholder(start_pos);
            }
            '#' => {
                // H40330: SQLite shall recognize as a VARIABLE token the sharp-sign followed
                // by a parameter name that does not begin with a NUMERIC character.
                return self.parse_variable_placeholder_sharp_sign(start_pos);
            }
            _ => {
                // Unrecognized character
                return Some(Err(TokenizerError::UnrecognizedToken));
            }
        };
        Some(Ok(Token {
            token_type,
            position: start_pos,
        }))
    }

    /// Reads the next token from the input
    pub fn next_token(&mut self) -> Option<Result<Token<'input>, TokenizerError<'input>>> {
        self.skip_whitespace();

        let start_pos = self.current_pos;

        // Peek at the next character
        let ch = self.next_char();

        match ch {
            Some(ch) => {
                // Match based on the character
                if ch == '`' {
                    self.parse_identifier_between_grave_accent(start_pos)
                } else if ch == '[' {
                    return self.parse_identifier_between_square_brackets(start_pos);
                } else if Self::is_id_start_char(&ch) {
                    return self.parse_literal(start_pos, ch);
                } else if ch.is_ascii_digit() {
                    // Numeric literal (integer or float)
                    return self.parse_numberic_literal(start_pos, ch);
                } else if ch == '\'' || ch == '"' {
                    // String literal
                    return self.parse_string_literal(start_pos, ch);
                } else if ch == '[' || ch == '`' {
                    // Identifier literal
                    return self.parse_string_literal(start_pos, ch);
                } else {
                    return self.parse_operators(start_pos, ch);
                }
            }
            None => match self.last_semi_token_pos {
                Some(pos) => {
                    if self.current_pos - pos > 1 {
                        self.last_semi_token_pos = Some(self.current_pos);
                        Some(Ok(Token {
                            token_type: TokenType::Semi,
                            position: self.current_pos,
                        }))
                    } else {
                        None
                    }
                }
                None => {
                    self.last_semi_token_pos = Some(self.current_pos);
                    Some(Ok(Token {
                        token_type: TokenType::Semi,
                        position: self.current_pos,
                    }))
                }
            },
        }
    }

    /// Collects all tokens into a vector, or returns an error if any tokenization error occurs
    pub fn tokens(self) -> Result<Vec<Token<'input>>, TokenizerError<'input>> {
        self.into()
    }

    /// Collects all tokens into a vector, or returns an error if any tokenization error occurs
    /// alias for `tokens` method
    pub fn collect(self) -> Result<Vec<Token<'input>>, TokenizerError<'input>> {
        self.tokens()
    }
}

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(text: &'a str) -> Self {
        Tokenizer {
            raw_content: text,
            chars: text.chars().peekable(),
            current_pos: 0,
            last_semi_token_pos: None,
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token<'input>, TokenizerError<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'input> From<Tokenizer<'input>> for Result<Vec<Token<'input>>, TokenizerError<'input>> {
    fn from(tokenizer: Tokenizer<'input>) -> Self {
        tokenizer.into_iter().collect()
    }
}

#[cfg(test)]
pub mod test_utils {
    use crate::{TokenType, Tokenizer};

    use super::TokenizerError;

    pub fn run_sunny_day_test<'a>(sql: &'a str, expected_tokens: Vec<TokenType<'a>>) {
        let mut tokenizer = Tokenizer::from(sql);

        for expected_token_type in expected_tokens {
            let token = tokenizer.next_token();
            let token = token.expect("Expected a token, but got None");
            let token = token.unwrap_or_else(|_| {
                panic!("Expected {:?}, got Unexpected error: ", expected_token_type)
            });

            // Verify that the token type matches the expected token
            assert_eq!(
                token.token_type, expected_token_type,
                "Expected token {:?}, got {:?}",
                expected_token_type, token.token_type
            );
        }

        // Ensure there are no more tokens
        assert!(
            tokenizer.next_token().is_none(),
            "Tokenizer should have no more tokens"
        );
    }

    pub fn run_rainy_day_test<'a>(
        sql: &'a str,
        expected_tokens: Vec<TokenType<'a>>,
        expected_error: TokenizerError,
    ) {
        let mut tokenizer = Tokenizer::from(sql);

        // We expect tokens up until the comment, then an error
        for expected_token_type in expected_tokens {
            let token = tokenizer.next_token();
            let token = token.expect("Expected a token, but got None");
            let token = token.unwrap_or_else(|_| {
                panic!("Expected {:?}, got Unexpected error: ", expected_token_type)
            });

            assert_eq!(
                token.token_type, expected_token_type,
                "Expected token {:?}, got {:?}",
                expected_token_type, token.token_type
            );
        }

        // The next call to next_token() should return an error
        let token: Option<Result<crate::Token<'_>, TokenizerError<'_>>> = tokenizer.next_token();
        let token = token.expect("Expected a token, but got None");
        let token_status = token.expect_err("Expected an error, but got a token");

        if token_status != expected_error {
            assert_eq!(
                token_status, expected_error,
                "Expected {:?}, got {:?}",
                expected_error, token_status,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenizerError;
    use crate::keywords::KEYWORD_MAP;
    use crate::tokenizer::test_utils::{run_rainy_day_test, run_sunny_day_test};
    use crate::{Keyword, TokenType};

    #[test]
    fn test_empty_input() {
        run_sunny_day_test("", vec![TokenType::Semi]);
    }

    #[test]
    fn test_single_keyword() {
        KEYWORD_MAP.iter().for_each(|(k, v)| {
            let sql = k.to_string();
            let expected_tokens = vec![TokenType::Keyword(*v), TokenType::Semi];
            run_sunny_day_test(&sql, expected_tokens);
        });
    }

    #[test]
    fn test_multiple_keywords() {
        let sql = "SELECT FROM WHERE;";

        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Keyword(Keyword::From),
            TokenType::Keyword(Keyword::Where),
            TokenType::Semi,
        ];

        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_case_unsensetive_keywords() {
        let sql = "Select select SELECT sElEcT SeLeCt;";

        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Keyword(Keyword::Select),
            TokenType::Keyword(Keyword::Select),
            TokenType::Keyword(Keyword::Select),
            TokenType::Keyword(Keyword::Select),
            TokenType::Semi,
        ];

        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_identifier() {
        run_sunny_day_test(
            "my_table abc012;",
            vec![
                TokenType::Id("my_table"),
                TokenType::Id("abc012"),
                TokenType::Semi,
            ],
        );

        // tokenize unicode identifiers
        run_sunny_day_test(
            "таблица Столбец",
            vec![
                TokenType::Id("таблица"),
                TokenType::Id("Столбец"),
                TokenType::Semi,
            ],
        );

        // tokenize identifiers between grave accents
        run_sunny_day_test(
            "`table_name` `id``entifier`",
            vec![
                TokenType::Id("`table_name`"),
                TokenType::Id("`id``entifier`"),
                TokenType::Semi,
            ],
        );

        run_rainy_day_test("`unterminated", vec![], TokenizerError::UnexpectedEOF);
        run_rainy_day_test("`abc``def``", vec![], TokenizerError::UnexpectedEOF);

        // tokenize identifiers between [square-brackets]
        run_sunny_day_test(
            "[table_name] [id``entifier] [123123] [^!@%#!@*$!@]",
            vec![
                TokenType::Id("[table_name]"),
                TokenType::Id("[id``entifier]"),
                TokenType::Id("[123123]"),
                TokenType::Id("[^!@%#!@*$!@]"),
                TokenType::Semi,
            ],
        );

        run_rainy_day_test("[unterminated", vec![], TokenizerError::UnexpectedEOF);
        run_rainy_day_test(
            "[abc]]",
            vec![TokenType::Id("[abc]")],
            TokenizerError::UnrecognizedToken,
        );
    }

    #[test]
    fn test_boolean_literals() {
        run_sunny_day_test("true", vec![TokenType::True, TokenType::Semi]);
        run_sunny_day_test("false", vec![TokenType::False, TokenType::Semi]);
        run_sunny_day_test("true1", vec![TokenType::Id("true1"), TokenType::Semi]);
        run_sunny_day_test("fal_se", vec![TokenType::Id("fal_se"), TokenType::Semi]);
    }

    #[test]
    fn test_integer_literals() {
        let sql = "0 42 123 999999999999999999999999999999999 ;";
        let expected_tokens = vec![
            TokenType::Integer("0"),
            TokenType::Integer("42"),
            TokenType::Integer("123"),
            TokenType::Integer("999999999999999999999999999999999"),
            TokenType::Semi,
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "0 -42 -123 -999999999999999999999999999999999 ";
        let expected_tokens = vec![
            TokenType::Integer("0"),
            TokenType::Minus,
            TokenType::Integer("42"),
            TokenType::Minus,
            TokenType::Integer("123"),
            TokenType::Minus,
            TokenType::Integer("999999999999999999999999999999999"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "0_0 1_000 1_000_000 -42_000 -123_00 -999999999999999999999999999999999_0 ";
        let expected_tokens = vec![
            TokenType::Integer("0_0"),
            TokenType::Integer("1_000"),
            TokenType::Integer("1_000_000"),
            TokenType::Minus,
            TokenType::Integer("42_000"),
            TokenType::Minus,
            TokenType::Integer("123_00"),
            TokenType::Minus,
            TokenType::Integer("999999999999999999999999999999999_0"),
            TokenType::Semi,
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "0x1 0x1234567890ABCDF 0xFFFF";
        let expected_tokens = vec![
            TokenType::Integer("0x1"),
            TokenType::Integer("0x1234567890ABCDF"),
            TokenType::Integer("0xFFFF"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        run_rainy_day_test("100_", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("100__", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("1__00", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("0abc", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("0xABCDFG", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("0xEEEXZY", vec![], TokenizerError::BadNumber);
    }

    #[test]
    fn test_float_literals() {
        let sql = "0.0 123.456 3.14 ";

        let expected_tokens = vec![
            TokenType::Float("0.0"),
            TokenType::Float("123.456"),
            TokenType::Float("3.14"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "1e10 3.14E-2 2.5e+3 ";
        let expected_tokens = vec![
            TokenType::Float("1e10"),
            TokenType::Float("3.14E-2"),
            TokenType::Float("2.5e+3"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "42.0 3.1415 1e-4";
        let expected_tokens = vec![
            TokenType::Float("42.0"),
            TokenType::Float("3.1415"),
            TokenType::Float("1e-4"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        let sql = "-42.0 -0.1 -1e-4";
        let expected_tokens = vec![
            TokenType::Minus,
            TokenType::Float("42.0"),
            TokenType::Minus,
            TokenType::Float("0.1"),
            TokenType::Minus,
            TokenType::Float("1e-4"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        run_rainy_day_test("123.abc ", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("123.0abc ", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("12.34.56", vec![], TokenizerError::BadNumber);

        run_rainy_day_test("1e", vec![], TokenizerError::BadNumber);
        run_rainy_day_test("3.14e1.5", vec![], TokenizerError::BadNumber);
    }

    #[test]
    fn test_string_literal_single_quotes() {
        run_sunny_day_test(
            "'hello' ;",
            vec![TokenType::String("'hello'"), TokenType::Semi],
        );
        run_sunny_day_test("'';", vec![TokenType::String("''"), TokenType::Semi]);
        run_sunny_day_test("'1';", vec![TokenType::String("'1'"), TokenType::Semi]);
        run_sunny_day_test(
            "'He said ''Hey, Marta!'''",
            vec![
                TokenType::String("'He said ''Hey, Marta!'''"),
                TokenType::Semi,
            ],
        );
    }

    #[test]
    fn test_string_literal_double_quotes() {
        run_sunny_day_test(
            "\"hello world\";",
            vec![TokenType::Id("\"hello world\""), TokenType::Semi],
        );

        run_sunny_day_test(
            "\"\";",
            vec![
                TokenType::Id("\"\""),
                TokenType::Semi, // Implicitly ; added
            ],
        );
        run_sunny_day_test(
            "\"1\";",
            vec![
                TokenType::Id("\"1\""),
                TokenType::Semi, // Implicitly ; added
            ],
        );
        run_sunny_day_test(
            "\"He said \"\"Hey, Marta!\"\"\"",
            vec![
                TokenType::Id(r#""He said ""Hey, Marta!""""#),
                TokenType::Semi, // Implicitly ; added
            ],
        );

        run_sunny_day_test(
            "'Line\\nBreak'",
            vec![
                TokenType::String("'Line\\nBreak'"),
                TokenType::Semi, // Implicitly ; added
            ],
        );
    }

    #[test]
    fn test_unclosed_string_literal() {
        run_rainy_day_test(
            "'unclosed string",
            vec![],
            TokenizerError::UnterminatedLiteral("'unclosed string"),
        );
    }

    #[test]
    fn test_single_character_operators() {
        let operators = "+ - * / % & ~ | = == < >";
        let expected_tokens = vec![
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Remainder,
            TokenType::BitAnd,
            TokenType::BitNot,
            TokenType::BitOr,
            TokenType::Equals,
            TokenType::EqualsEquals,
            TokenType::LessThan,
            TokenType::GreaterThan,
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(operators, expected_tokens);
    }

    #[test]
    fn test_multi_character_operators() {
        let operators = "== != >= <= <> << >> ||";
        let expected_tokens = vec![
            TokenType::EqualsEquals,
            TokenType::NotEquals,
            TokenType::GreaterEquals,
            TokenType::LessEquals,
            TokenType::NotEquals,
            TokenType::LeftShift,
            TokenType::RightShift,
            TokenType::Concat,
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(operators, expected_tokens);
    }

    #[test]
    fn test_punctuation() {
        let punctuations = ", ; ( )";
        let expected_tokens = vec![
            TokenType::Comma,
            TokenType::Semi,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::Semi, // Implicitly ; added
        ];

        run_sunny_day_test(punctuations, expected_tokens);
    }

    #[test]
    fn test_whitespace_handling() {
        let sql = "SELECT   \t\n  *  FROM   users\n\t";
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Star,
            TokenType::Keyword(Keyword::From),
            TokenType::Id("users"),
            TokenType::Semi, // Implicitly ; added
        ];

        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_single_line_comments() {
        let sql = "SELECT * FROM users -- This is a comment\nWHERE id = 1;";
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Star,
            TokenType::Keyword(Keyword::From),
            TokenType::Id("users"),
            TokenType::SingleLineComment(" This is a comment\n"),
            TokenType::Keyword(Keyword::Where),
            TokenType::Id("id"),
            TokenType::Equals,
            TokenType::Integer("1"),
            TokenType::Semi,
        ];
        run_sunny_day_test(sql, expected_tokens);

        run_sunny_day_test(
            "/* SELECT * FROM table */",
            vec![
                TokenType::MultiLineComment(" SELECT * FROM table "),
                TokenType::Semi, // Implicitly ; added
            ],
        );
    }

    #[test]
    fn test_multi_line_comments() {
        let sql = "SELECT /* This is a \nmulti-line comment */ * FROM users;";
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::MultiLineComment(" This is a \nmulti-line comment "),
            TokenType::Star,
            TokenType::Keyword(Keyword::From),
            TokenType::Id("users"),
            TokenType::Semi,
        ];
        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_unterminated_multi_line_comment() {
        let sql = "SELECT * FROM users /* Unterminated comment";

        // We expect tokens up until the comment, then an error
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Star,
            TokenType::Keyword(Keyword::From),
            TokenType::Id("users"),
        ];

        run_rainy_day_test(
            sql,
            expected_tokens,
            TokenizerError::UnterminatedCommentBlock,
        );
    }

    #[test]
    fn test_variable_placeholders() {
        // TODO: Support ?-1 ?-1.2 ?123abc in the tokenizer
        // (double check with the spec, as sqlite supports it. Dunno if it's a bug or a feature)
        // let sql = "? ?1 ?abc ?1abc ?-1 ?-1.2 ?123abc :name :123 @var $value #param";
        let sql = "? ?1 :name @var $value #param";
        let expected_tokens = vec![
            TokenType::Variable("?"),
            TokenType::Variable("?1"),
            TokenType::Variable(":name"),
            TokenType::Variable("@var"),
            TokenType::Variable("$value"),
            TokenType::Variable("#param"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);

        let invalid_test_cases = vec![
            (":123", vec![], TokenizerError::BadVariableName),
            (": ", vec![], TokenizerError::BadVariableName),
            ("@ ", vec![], TokenizerError::BadVariableName),
            ("$ ", vec![], TokenizerError::BadVariableName),
            ("# ", vec![], TokenizerError::BadVariableName),
            ("?abc", vec![], TokenizerError::BadVariableName),
            ("?1abc", vec![], TokenizerError::BadVariableName),
            ("?$", vec![], TokenizerError::BadVariableName),
            ("?-1", vec![], TokenizerError::BadVariableName),
            ("?1.2", vec![], TokenizerError::BadVariableName),
            ("?123abc", vec![], TokenizerError::BadVariableName),
        ];

        for (sql, tokens, expected_error) in invalid_test_cases {
            run_rainy_day_test(sql, tokens, expected_error);
        }
    }

    #[test]
    fn test_blob_literals() {
        run_sunny_day_test(
            "x'1A2B';",
            vec![TokenType::Blob("x'1A2B'"), TokenType::Semi],
        );
        run_rainy_day_test(
            "x'1A2B' x'ZZ'",
            vec![TokenType::Blob("x'1A2B'")],
            TokenizerError::MalformedBlobLiteral("x'ZZ'", 10),
        );
    }

    #[test]
    fn test_unrecognized_tokens() {
        run_rainy_day_test("^", vec![], TokenizerError::UnrecognizedToken);
    }

    #[test]
    fn test_sql_injection() {
        use Keyword::*;
        let sql = "SELECT * FROM users WHERE ' OR '1'='1";
        let expected_tokens = vec![
            TokenType::Keyword(Select),
            TokenType::Star,
            TokenType::Keyword(From),
            TokenType::Id("users"),
            TokenType::Keyword(Where),
            TokenType::String("' OR '"),
            TokenType::Integer("1"),
            TokenType::String("'='"),
            TokenType::Integer("1"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_literal_and_keyword() {
        use Keyword::*;
        let sql = "SELECT all \"name\"as name";
        let expected_tokens = vec![
            TokenType::Keyword(Select),
            TokenType::Keyword(All),
            TokenType::Id("\"name\""),
            TokenType::Keyword(As),
            TokenType::Id("name"),
            TokenType::Semi, // Implicitly ; added
        ];
        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_explicit_semi_at_the_end() {
        let sql = "SELECT 1;";
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Integer("1"),
            TokenType::Semi,
        ];
        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_implicit_semi_at_the_end() {
        let sql = "SELECT 1";
        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Select),
            TokenType::Integer("1"),
            TokenType::Semi, // Implicit semicolon added
        ];
        run_sunny_day_test(sql, expected_tokens);
    }

    #[test]
    fn test_semi_and_new_line() {
        run_sunny_day_test(
            "SELECT ; FROM users --Comment without newline ",
            vec![
                TokenType::Keyword(Keyword::Select),
                TokenType::Semi,
                TokenType::Keyword(Keyword::From),
                TokenType::Id("users"),
                TokenType::SingleLineComment("Comment without newline "),
                TokenType::Semi,
            ],
        )
    }

    #[test]
    fn test_semi_and_new_line1() {
        use Keyword::*;

        run_sunny_day_test(
            "SELECT * FROM users; -- This is a comment\nSELECT * FROM orders;",
            vec![
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("users"),
                TokenType::Semi,
                TokenType::SingleLineComment(" This is a comment\n"),
                TokenType::Keyword(Select),
                TokenType::Star,
                TokenType::Keyword(From),
                TokenType::Id("orders"),
                TokenType::Semi,
            ],
        )
    }
}
