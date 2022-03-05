use crate::toolkit::{TokenRecognizer, TokenRecognizerError};
use memchr::memchr2;
use oxilangtag::LanguageTag;
use oxiri::Iri;
use std::ops::Range;
use std::str;

// TODO: test TTL file "@prefix :<http://example.com>.:::."
// TODO: fun stuff with '..'?

const ALLOWED_SPARQL_KEYWORDS: [&str; 2] = ["PREFIX", "BASE"];
const ALLOWED_PLAIN_KEYWORDS: [&str; 5] = ["a", "has", "is", "of", "id"];
const ALLOWED_AT_KEYWORDS: [&str; 2] = ["prefix", "base"];

#[derive(Debug, PartialEq, Eq)]
pub enum N3Token<'a> {
    IriRef(Iri<String>),
    PrefixedName(&'a str, &'a str),
    Variable(&'a str),
    BlankNodeLabel(&'a str),
    String(String),
    Integer(&'a str),
    Decimal(&'a str),
    Double(&'a str),
    Boolean(bool),
    LangTag(LanguageTag<String>),
    Punctuation(&'a str),
    PlainKeyword(&'a str),
    AtKeyword(&'a str),
}

#[derive(Eq, PartialEq)]
pub enum N3LexerMode {
    NTriples,
    Turtle,
    N3,
}

#[derive(Default)]
pub struct N3LexerOptions {
    pub base_iri: Option<Iri<String>>,
}

pub struct N3Lexer {
    mode: N3LexerMode,
}

impl N3Lexer {
    pub fn new(mode: N3LexerMode) -> Self {
        Self { mode }
    }
}

// TODO: there are a lot of 'None' (missing data) returned even if the stram is ending!!!
// TODO: simplify by not giving is_end and fail with an "unexpected eof" is none is returned when is_end=true?
// TODO: avoid copies inside tokens when GAT is going to be stable

impl TokenRecognizer for N3Lexer {
    type Token<'a> = N3Token<'a>;
    type Options = N3LexerOptions;

    fn recognize_next<'a>(
        &mut self,
        data: &'a [u8],
        is_ending: bool,
        options: &N3LexerOptions,
    ) -> Option<(usize, Result<N3Token<'a>, TokenRecognizerError>)> {
        match *data.first()? {
            b'<' => match *data.get(1)? {
                b'<' => Some((2, Ok(N3Token::Punctuation("<<")))),
                b'=' => Some((2, Ok(N3Token::Punctuation("<=")))),
                //TODO b'-' => Some((2, Ok(N3Token::Punctuation("<-")))),
                _ => self.recognize_iri(data, options),
            },
            b'>' => {
                if *data.get(1)? == b'>' {
                    Some((2, Ok(N3Token::Punctuation(">>"))))
                } else {
                    Some((1, Ok(N3Token::Punctuation(">"))))
                }
            }
            b'_' => match data.get(1)? {
                b':' => self.recognize_blank_node_label(data),
                c => Some((
                    1,
                    Err((0, format!("Unexpected character '{}'", char::from(*c))).into()),
                )),
            },
            b'"' => {
                if self.mode != N3LexerMode::NTriples
                    && *data.get(1)? == b'"'
                    && *data.get(2)? == b'"'
                {
                    self.recognize_long_string(data, b'"')
                } else {
                    self.recognize_string(data, b'"')
                }
            }
            b'\'' if self.mode != N3LexerMode::NTriples => {
                if *data.get(1)? == b'\'' && *data.get(2)? == b'\'' {
                    self.recognize_long_string(data, b'\'')
                } else {
                    self.recognize_string(data, b'\'')
                }
            }
            b'@' => self.recognize_langtag_or_keyword(data),
            b'.' => match data.get(1) {
                Some(b'0'..=b'9') => self.recognize_number(data),
                Some(_) => Some((1, Ok(N3Token::Punctuation(".")))),
                None => {
                    if is_ending {
                        Some((1, Ok(N3Token::Punctuation("."))))
                    } else {
                        None
                    }
                }
            },
            b'^' => {
                if *data.get(1)? == b'^' {
                    Some((2, Ok(N3Token::Punctuation("^^"))))
                } else {
                    Some((1, Ok(N3Token::Punctuation("^"))))
                }
            }
            b'(' => Some((1, Ok(N3Token::Punctuation("(")))),
            b')' => Some((1, Ok(N3Token::Punctuation(")")))),
            b'[' => Some((1, Ok(N3Token::Punctuation("[")))),
            b']' => Some((1, Ok(N3Token::Punctuation("]")))),
            b'{' => Some((1, Ok(N3Token::Punctuation("{")))),
            b'}' => Some((1, Ok(N3Token::Punctuation("}")))),
            b',' => Some((1, Ok(N3Token::Punctuation(",")))),
            b';' => Some((1, Ok(N3Token::Punctuation(";")))),
            b'!' => Some((1, Ok(N3Token::Punctuation("!")))),
            b'=' => {
                if *data.get(1)? == b'>' {
                    Some((2, Ok(N3Token::Punctuation("=>"))))
                } else {
                    Some((1, Ok(N3Token::Punctuation("="))))
                }
            }
            b'0'..=b'9' | b'+' | b'-' => self.recognize_number(data),
            b'?' => self.recognize_variable(data),
            _ => self.recognize_pname_or_keyword(data),
        }
    }
}

impl N3Lexer {
    fn recognize_iri(
        &self,
        data: &[u8],
        options: &N3LexerOptions,
    ) -> Option<(usize, Result<N3Token<'static>, TokenRecognizerError>)> {
        // [18] 	IRIREF 	::= 	'<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
        let mut string = Vec::new();
        let mut i = 1;
        loop {
            let end = memchr2(b'>', b'\\', &data[i..])?;
            string.extend_from_slice(&data[i..i + end]);
            i += end;
            match data[i] {
                b'>' => {
                    return Some((i + 1, self.parse_iri(string, 0..i + 1, options)));
                }
                b'\\' => {
                    let (additional, c) = Self::recognize_escape(&data[i..], i, false)?;
                    i += additional + 1;
                    match c {
                        Ok(c) => {
                            let mut buf = [0; 4];
                            string.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                        }
                        Err(e) => return Some((i, Err(e))),
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn parse_iri(
        &self,
        iri: Vec<u8>,
        position: Range<usize>,
        options: &N3LexerOptions,
    ) -> Result<N3Token<'static>, TokenRecognizerError> {
        let iri = String::from_utf8(iri).map_err(|e| {
            (
                position.clone(),
                format!("The IRI contains invalid UTF-8 characters: {}", e),
            )
        })?;
        let iri = if let Some(base_iri) = options.base_iri.as_ref() {
            base_iri.resolve(&iri)
        } else {
            Iri::parse(iri)
        }
        .map_err(|e| (position, e.to_string()))?;
        Ok(N3Token::IriRef(iri))
    }

    fn recognize_pname_or_keyword<'a>(
        &self,
        data: &'a [u8],
    ) -> Option<(usize, Result<N3Token<'a>, TokenRecognizerError>)> {
        // [139s] 	PNAME_NS 	::= 	PN_PREFIX? ':'
        // [140s] 	PNAME_LN 	::= 	PNAME_NS PN_LOCAL

        // [167s] 	PN_PREFIX 	::= 	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
        let mut i = 0;
        loop {
            match Self::recognize_unicode_char(&data[i..], i)? {
                Ok((c, consumed)) => {
                    if c == ':' {
                        i += consumed;
                        break;
                    } else if i == 0 {
                        if !Self::is_possible_pn_chars_base(c) {
                            return Some((
                                consumed,
                                Err((
                                    0..consumed,
                                    format!(
                                        "'{}' is not allowed at the beginning of a prefix name",
                                        c
                                    ),
                                )
                                    .into()),
                            ));
                        }
                        i += consumed;
                    } else if Self::is_possible_pn_chars(c) || c == '.' {
                        i += consumed;
                    } else {
                        while data.get(i - 1).copied() == Some(b'.') {
                            i -= 1;
                        }
                        let pn_prefix = match Self::str_from_bytes(&data[..i], "keyword", 0..i) {
                            Ok(pn_prefix) => pn_prefix,
                            Err(e) => return Some((i, Err(e))),
                        };
                        for keyword in ALLOWED_SPARQL_KEYWORDS {
                            if pn_prefix.eq_ignore_ascii_case(keyword) {
                                return Some((i, Ok(N3Token::PlainKeyword(keyword))));
                            }
                        }
                        for keyword in ALLOWED_PLAIN_KEYWORDS {
                            if pn_prefix == keyword {
                                return Some((i, Ok(N3Token::PlainKeyword(keyword))));
                            }
                        }
                        if pn_prefix == "true" {
                            return Some((i, Ok(N3Token::Boolean(true))));
                        }
                        if pn_prefix == "false" {
                            return Some((i, Ok(N3Token::Boolean(false))));
                        }
                        return Some((
                            i,
                            Err((0..i, format!("'{}' is not a valid keyword", pn_prefix)).into()),
                        ));
                    }
                }
                Err(e) => return Some((i + e.position.end, Err(e))),
            }
        }
        let pn_prefix = match Self::str_from_bytes(&data[..i - 1], "prefix name", 0..i - 1) {
            Ok(pn_prefix) => pn_prefix,
            Err(e) => return Some((i, Err(e))),
        };
        if pn_prefix.ends_with('.') {
            return Some((
                i,
                Err((
                    0..i,
                    format!(
                        "'{}' is not a valid prefix: prefixes are not allowed to end with '.'",
                        pn_prefix
                    ),
                )
                    .into()),
            ));
        }

        let (consumed, pn_local_result) = self.recognize_optional_pn_local(&data[i..])?;
        Some((
            consumed + i,
            pn_local_result.map(|pn_local| N3Token::PrefixedName(pn_prefix, pn_local)),
        ))
    }

    fn recognize_variable<'a>(
        &self,
        data: &'a [u8],
    ) -> Option<(usize, Result<N3Token<'a>, TokenRecognizerError>)> {
        // [36] 	QUICK_VAR_NAME 	::= 	"?" PN_LOCAL
        let (consumed, result) = self.recognize_optional_pn_local(&data[1..])?;
        Some((
            consumed + 1,
            result.and_then(|name| {
                if name.is_empty() {
                    Err((0..consumed, "A variable name is not allowed to be empty").into())
                } else {
                    Ok(N3Token::Variable(name))
                }
            }),
        ))
    }

    fn recognize_optional_pn_local<'a>(
        &self,
        data: &'a [u8],
    ) -> Option<(usize, Result<&'a str, TokenRecognizerError>)> {
        // [168s] 	PN_LOCAL 	::= 	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
        let mut i = 0;
        loop {
            match Self::recognize_unicode_char(&data[i..], i)? {
                Ok((c, consumed)) => {
                    if c == '%' {
                        i += 1;
                        let a = char::from(*data.get(i)?);
                        i += 1;
                        let b = char::from(*data.get(i)?);
                        if !matches!(a, '0'..='9' | 'A'..='F' | 'a'..='f')
                            || !matches!(b, '0'..='9' | 'A'..='F' | 'a'..='f')
                        {
                            return Some((i+1, Err((
                                i-2..i+1, format!("escapes in IRIs should be % followed by two hexadecimal characters, found '%{}{}'", a, b)
                            ).into())));
                        }
                        i += 1;
                    } else if c == '\\' {
                        i += 1;
                        let a = char::from(*data.get(i)?);
                        if !matches!(
                            a,
                            '_' | '~'
                                | '.'
                                | '-'
                                | '!'
                                | '$'
                                | '&'
                                | '\''
                                | '('
                                | ')'
                                | '*'
                                | '+'
                                | ','
                                | ';'
                                | '='
                                | '/'
                                | '?'
                                | '#'
                                | '@'
                                | '%'
                        ) {
                            return Some((i + 1, Err((
                                i..i+1, format!("The character that are allowed to be escaped in IRIs are _~.-!$&'()*+,;=/?#@%, found '{}'", a)
                            ).into())));
                        }
                        i += 1;
                    } else if i == 0 {
                        if !(Self::is_possible_pn_chars_u(c) || c == ':' || matches!(c, '0'..='9'))
                        {
                            return Some((0, Ok("")));
                        }
                        i += consumed;
                    } else if Self::is_possible_pn_chars(c) || c == ':' || c == '.' {
                        i += consumed;
                    } else {
                        // We do not include the last dot
                        while data.get(i - 1).copied() == Some(b'.') {
                            i -= 1;
                        }
                        return Some((i, Self::str_from_bytes(&data[..i], "local name", 0..i)));
                    }
                }
                Err(e) => return Some((i + e.position.end, Err(e))),
            }
        }
    }

    fn recognize_blank_node_label<'a>(
        &self,
        data: &'a [u8],
    ) -> Option<(usize, Result<N3Token<'a>, TokenRecognizerError>)> {
        // [141s] 	BLANK_NODE_LABEL 	::= 	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
        let mut i = 2;
        loop {
            match Self::recognize_unicode_char(&data[i..], i)? {
                Ok((c, consumed)) => {
                    if (i == 2 && (Self::is_possible_pn_chars_u(c) || matches!(c, '0'..='9')))
                        || (i > 2 && Self::is_possible_pn_chars(c))
                    {
                        // Ok
                    } else if i > 2 && c == '.' {
                        if data[i - 1] == b'.' {
                            i -= 1;
                            return Some((
                                i,
                                Self::str_from_bytes(&data[..i], "blank node id", 0..i)
                                    .map(N3Token::BlankNodeLabel),
                            ));
                        }
                    } else if i == 0 {
                        return Some((
                            i,
                            Err((0..i, "A blank node ID should not be empty").into()),
                        ));
                    } else if data[i - 1] == b'.' {
                        i -= 1;
                        return Some((
                            i,
                            Self::str_from_bytes(&data[..i], "blank node id", 0..i)
                                .map(N3Token::BlankNodeLabel),
                        ));
                    } else {
                        return Some((
                            i,
                            Self::str_from_bytes(&data[..i], "blank node id", 0..i)
                                .map(N3Token::BlankNodeLabel),
                        ));
                    }
                    i += consumed;
                }
                Err(e) => return Some((i + e.position.end, Err(e))),
            }
        }
    }

    fn recognize_langtag_or_keyword(
        &self,
        data: &[u8],
    ) -> Option<(usize, Result<N3Token<'static>, TokenRecognizerError>)> {
        // [144s] 	LANGTAG 	::= 	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
        let mut is_last_block_empty = true;
        for (i, c) in data[1..].iter().enumerate() {
            if matches!(c, b'a'..=b'z' | b'A'..=b'Z') {
                is_last_block_empty = false;
            } else if i == 0 {
                return Some((
                    1,
                    Err((1..2, "A language code should always start with a letter").into()),
                ));
            } else if is_last_block_empty {
                return Some((i, Self::parse_lang_tag_or_keyword(&data[1..i], 1..i - 1)));
            } else if *c == b'-' {
                is_last_block_empty = true;
            } else {
                return Some((
                    i + 1,
                    Self::parse_lang_tag_or_keyword(&data[1..i + 1], 1..i),
                ));
            }
        }
        None
    }

    fn parse_lang_tag_or_keyword(
        lang_tag_or_keyword: &[u8],
        position: Range<usize>,
    ) -> Result<N3Token<'static>, TokenRecognizerError> {
        let lang_tag_or_keyword =
            Self::str_from_bytes(lang_tag_or_keyword, "language tag", position.clone())?;
        for keyword in ALLOWED_AT_KEYWORDS {
            if lang_tag_or_keyword == keyword {
                return Ok(N3Token::AtKeyword(keyword));
            }
        }
        let lang_tag = LanguageTag::parse(lang_tag_or_keyword.to_ascii_lowercase())
            .map_err(|e| (position.clone(), e.to_string()))?;
        Ok(N3Token::LangTag(lang_tag))
    }

    fn recognize_string(
        &self,
        data: &[u8],
        delimiter: u8,
    ) -> Option<(usize, Result<N3Token<'static>, TokenRecognizerError>)> {
        // [22] 	STRING_LITERAL_QUOTE 	::= 	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"' /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
        // [23] 	STRING_LITERAL_SINGLE_QUOTE 	::= 	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
        let mut string = Vec::new();
        let mut i = 1;
        loop {
            let end = memchr2(delimiter, b'\\', &data[i..])?;
            string.extend_from_slice(&data[i..i + end]);
            i += end;
            match data[i] {
                c if c == delimiter => {
                    return Some((i + 1, Self::parse_string(string, 0..i + 1)));
                }
                b'\\' => {
                    let (additional, c) = Self::recognize_escape(&data[i..], i, true)?;
                    i += additional + 1;
                    match c {
                        Ok(c) => {
                            let mut buf = [0; 4];
                            string.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                        }
                        Err(e) => return Some((i, Err(e))),
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn recognize_long_string(
        &self,
        data: &[u8],
        delimiter: u8,
    ) -> Option<(usize, Result<N3Token<'static>, TokenRecognizerError>)> {
        // [24] 	STRING_LITERAL_LONG_SINGLE_QUOTE 	::= 	"'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
        // [25] 	STRING_LITERAL_LONG_QUOTE 	::= 	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
        let mut string = Vec::new();
        let mut i = 3;
        loop {
            let end = memchr2(delimiter, b'\\', &data[i..])?;
            string.extend_from_slice(&data[i..i + end]);
            i += end;
            match data[i] {
                c if c == delimiter => {
                    if *data.get(i + 1)? == delimiter && *data.get(i + 2)? == delimiter {
                        return Some((i + 3, Self::parse_string(string, 0..i + 3)));
                    } else {
                        i += 1;
                        string.push(delimiter);
                    }
                }
                b'\\' => {
                    let (additional, c) = Self::recognize_escape(&data[i..], i, true)?;
                    i += additional + 1;
                    match c {
                        Ok(c) => {
                            let mut buf = [0; 4];
                            string.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                        }
                        Err(e) => return Some((i, Err(e))),
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn parse_string(
        string: Vec<u8>,
        position: Range<usize>,
    ) -> Result<N3Token<'static>, TokenRecognizerError> {
        let string = String::from_utf8(string).map_err(|e| {
            (
                position.clone(),
                format!("The string contains invalid UTF-8 characters: {}", e),
            )
        })?;
        Ok(N3Token::String(string))
    }

    fn recognize_number<'a>(
        &self,
        data: &'a [u8],
    ) -> Option<(usize, Result<N3Token<'a>, TokenRecognizerError>)> {
        // [19] 	INTEGER 	::= 	[+-]? [0-9]+
        // [20] 	DECIMAL 	::= 	[+-]? [0-9]* '.' [0-9]+
        // [21] 	DOUBLE 	::= 	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
        // [154s] 	EXPONENT 	::= 	[eE] [+-]? [0-9]+
        let mut value = String::new();
        let mut i = 0;
        let c = *data.first()?;
        if matches!(c, b'+' | b'-') {
            value.push(char::from(c));
            i += 1;
        }
        // We read the digits before .
        let mut count_before: usize = 0;
        loop {
            let c = *data.get(i)?;
            if matches!(c, b'0'..=b'9') {
                value.push(char::from(c));
                i += 1;
                count_before += 1;
            } else {
                break;
            }
        }

        // We read the digits after .
        let count_after = if *data.get(i)? == b'.' {
            value.push('.');
            i += 1;

            let mut count_after = 0;
            loop {
                let c = *data.get(i)?;
                if matches!(c, b'0'..=b'9') {
                    value.push(char::from(c));
                    i += 1;
                    count_after += 1;
                } else {
                    break;
                }
            }
            Some(count_after)
        } else {
            None
        };

        // End
        let c = *data.get(i)?;
        if matches!(c, b'e' | b'E') {
            value.push(char::from(c));
            i += 1;

            let c = *data.get(i)?;
            if matches!(c, b'+' | b'-') {
                value.push(char::from(c));
                i += 1;
            }

            let mut found = false;
            loop {
                let c = *data.get(i)?;
                if matches!(c, b'0'..=b'9') {
                    value.push(char::from(c));
                    i += 1;
                    found = true;
                } else {
                    break;
                }
            }
            Some((
                i,
                if !found {
                    Err((0..i, "A double exponent cannot be empty").into())
                } else if count_before == 0 && count_after.unwrap_or(0) == 0 {
                    Err((0..i, "A double should not be empty").into())
                } else {
                    Self::str_from_bytes(&data[..i], "double", 0..i).map(N3Token::Double)
                },
            ))
        } else if let Some(count_after) = count_after {
            if count_after == 0 {
                // We do not consume the '.' after all
                value.pop();
                i -= 1;
                Some((
                    i,
                    if count_before == 0 {
                        Err((0..i, "An integer should not be empty").into())
                    } else {
                        Self::str_from_bytes(&data[..i], "integer", 0..i).map(N3Token::Integer)
                    },
                ))
            } else {
                Some((
                    i,
                    Self::str_from_bytes(&data[..i], "decimal", 0..i).map(N3Token::Decimal),
                ))
            }
        } else {
            Some((
                i,
                if count_before == 0 {
                    Err((0..i, "An integer should not be empty").into())
                } else {
                    Self::str_from_bytes(&data[..i], "integer", 0..i).map(N3Token::Integer)
                },
            ))
        }
    }

    fn str_from_bytes<'a>(
        bytes: &'a [u8],
        kind: &'static str,
        range: Range<usize>,
    ) -> Result<&'a str, TokenRecognizerError> {
        str::from_utf8(bytes).map_err(|e| {
            (
                range,
                format!("The {} contains invalid UTF-8 characters: {}", kind, e),
            )
                .into()
        })
    }

    fn recognize_escape(
        data: &[u8],
        position: usize,
        with_echar: bool,
    ) -> Option<(usize, Result<char, TokenRecognizerError>)> {
        // [26] 	UCHAR 	::= 	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
        // [159s] 	ECHAR 	::= 	'\' [tbnrf"'\]
        match *data.get(1)? {
            b'u' => match Self::recognize_hex_char(&data[2..], 4, 'u', position) {
                Ok(c) => Some((5, Ok(c?))),
                Err(e) => Some((5, Err(e))),
            },
            b'U' => match Self::recognize_hex_char(&data[2..], 8, 'u', position) {
                Ok(c) => Some((9, Ok(c?))),
                Err(e) => Some((9, Err(e))),
            },
            b't' if with_echar => Some((1, Ok('\t'))),
            b'b' if with_echar => Some((1, Ok('\x08'))),
            b'n' if with_echar => Some((1, Ok('\n'))),
            b'r' if with_echar => Some((1, Ok('\r'))),
            b'f' if with_echar => Some((1, Ok('\x0C'))),
            b'"' if with_echar => Some((1, Ok('"'))),
            b'\'' if with_echar => Some((1, Ok('\''))),
            b'\\' if with_echar => Some((1, Ok('\\'))),
            c => Some((
                1,
                Err((
                    position..position + 2,
                    format!("Unexpected escape character '\\{}'", char::from(c)),
                )
                    .into()),
            )), //TODO: read until end of string
        }
    }

    fn recognize_hex_char(
        data: &[u8],
        len: usize,
        escape_char: char,
        position: usize,
    ) -> Result<Option<char>, TokenRecognizerError> {
        if data.len() < len {
            return Ok(None);
        }
        let val =
            Self::str_from_bytes(&data[..len], "escapesequence", position..position + len + 2)?;
        let codepoint = u32::from_str_radix(val, 16).map_err(|e| {
            (
                position..position + len + 2,
                format!(
                    "The escape sequence '\\{}{}' is not a valid hexadecimal string: {}",
                    escape_char, val, e
                ),
            )
        })?;
        let c = char::from_u32(codepoint).ok_or_else(|| {
            (
                position..position + len +2,
                format!(
                    "The escape sequence '\\{}{}' is encoding {:X} that is not a valid unicode character",
                    escape_char, val, codepoint
                ),
            )
        })?;
        Ok(Some(c))
    }

    fn recognize_unicode_char(
        data: &[u8],
        position: usize,
    ) -> Option<Result<(char, usize), TokenRecognizerError>> {
        let mut code_point: u32;
        let bytes_needed: usize;
        let mut lower_boundary = 0x80;
        let mut upper_boundary = 0xBF;

        let byte = *data.first()?;
        match byte {
            0x00..=0x7F => return Some(Ok((char::from(byte), 1))),
            0xC2..=0xDF => {
                bytes_needed = 1;
                code_point = u32::from(byte) & 0x1F;
            }
            0xE0..=0xEF => {
                if byte == 0xE0 {
                    lower_boundary = 0xA0;
                }
                if byte == 0xED {
                    upper_boundary = 0x9F;
                }
                bytes_needed = 2;
                code_point = u32::from(byte) & 0xF;
            }
            0xF0..=0xF4 => {
                if byte == 0xF0 {
                    lower_boundary = 0x90;
                }
                if byte == 0xF4 {
                    upper_boundary = 0x8F;
                }
                bytes_needed = 3;
                code_point = u32::from(byte) & 0x7;
            }
            _ => {
                return Some(Err((
                    position..position + 1,
                    "Invalid UTF-8 character encoding",
                )
                    .into()))
            }
        }

        for i in 1..=bytes_needed {
            let byte = *data.get(i)?;
            if byte < lower_boundary || upper_boundary < byte {
                return Some(Err((
                    position..position + bytes_needed + 1,
                    "Invalid UTF-8 character encoding",
                )
                    .into()));
            }
            lower_boundary = 0x80;
            upper_boundary = 0xBF;
            code_point = (code_point << 6) | (u32::from(byte) & 0x3F);
        }

        Some(
            char::from_u32(code_point)
                .map(|c| (c, bytes_needed + 1))
                .ok_or_else(|| {
                    (
                        position..position + bytes_needed + 1,
                        format!(
                            "The codepoint {:X} is not a valid unicode character",
                            code_point
                        ),
                    )
                        .into()
                }),
        )
    }

    // [157s] 	PN_CHARS_BASE 	::= 	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    fn is_possible_pn_chars_base(c: char) -> bool {
        matches!(c,
        'A'..='Z'
        | 'a'..='z'
        | '\u{00C0}'..='\u{00D6}'
        | '\u{00D8}'..='\u{00F6}'
        | '\u{00F8}'..='\u{02FF}'
        | '\u{0370}'..='\u{037D}'
        | '\u{037F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}')
    }

    // [158s] 	PN_CHARS_U 	::= 	PN_CHARS_BASE | '_' | ':'
    fn is_possible_pn_chars_u(c: char) -> bool {
        Self::is_possible_pn_chars_base(c) || c == '_'
    }

    // [160s] 	PN_CHARS 	::= 	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
    fn is_possible_pn_chars(c: char) -> bool {
        Self::is_possible_pn_chars_u(c)
            || matches!(c,
        '-' | '0'..='9' | '\u{00B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
    }
}
