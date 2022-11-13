//! A [TriG](https://www.w3.org/TR/trig/) streaming parser implemented by [`TriGParser`].

use crate::terse::TriGRecognizer;
use crate::toolkit::{FromReadIterator, ParseError, ParseOrIoError, Parser};
use oxiri::{Iri, IriParseError};
use oxrdf::Quad;
use std::collections::HashMap;
use std::io::Read;

/// A [TriG](https://www.w3.org/TR/trig/) streaming parser.
///
/// Support for [TriG-star](https://w3c.github.io/rdf-star/cg-spec/#TriG-star) is available behind the `rdf-star` feature and the [`TriGParser::with_quoted_triples`] option.
///
/// Count the number of people:
/// ```
/// use oxrdf::NamedNodeRef;
/// use oxttl::{TriGParser, ParseError};
///
/// let file = b"@base <http://example.com/> .
/// @prefix schema: <http://schema.org/> .
/// <foo> a schema:Person ;
///     schema:name \"Foo\" .
/// <bar> a schema:Person ;
///     schema:name \"Bar\" .";
///
/// let rdf_type = NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
/// let schema_person = NamedNodeRef::new("http://schema.org/Person")?;
/// let mut count = 0;
/// for triple in TriGParser::new().parse_from_read(file.as_ref()) {
///     let triple = triple?;
///     if triple.predicate == rdf_type && triple.object == schema_person.into() {
///         count += 1;
///     }
/// }
/// assert_eq!(2, count);
/// # Result::<_,Box<dyn std::error::Error>>::Ok(())
/// ```
#[derive(Default)]
pub struct TriGParser {
    base: Option<Iri<String>>,
    prefixes: HashMap<String, Iri<String>>,
    #[cfg(feature = "rdf-star")]
    with_quoted_triples: bool,
}

impl TriGParser {
    /// Builds a new [`TriGParser`].
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_base_iri(mut self, base_iri: impl Into<String>) -> Result<Self, IriParseError> {
        self.base = Some(Iri::parse(base_iri.into())?);
        Ok(self)
    }

    #[inline]
    pub fn with_prefix(
        mut self,
        prefix_name: impl Into<String>,
        prefix_iri: impl Into<String>,
    ) -> Result<Self, IriParseError> {
        self.prefixes
            .insert(prefix_name.into(), Iri::parse(prefix_iri.into())?);
        Ok(self)
    }

    /// Enables [N-Triples-star](https://w3c.github.io/rdf-star/cg-spec/#TriG-star).
    #[cfg(feature = "rdf-star")]
    pub fn with_quoted_triples(mut self) -> Self {
        self.with_quoted_triples = true;
        self
    }

    /// Parses a TriG file from a [`Read`](std::io::Read) implementation.
    ///
    /// Count the number of people:
    /// ```
    /// use oxrdf::NamedNodeRef;
    /// use oxttl::{TriGParser, ParseError};
    ///
    /// let file = b"@base <http://example.com/> .
    /// @prefix schema: <http://schema.org/> .
    /// <foo> a schema:Person ;
    ///     schema:name \"Foo\" .
    /// <bar> a schema:Person ;
    ///     schema:name \"Bar\" .";
    ///
    /// let rdf_type = NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
    /// let schema_person = NamedNodeRef::new("http://schema.org/Person")?;
    /// let mut count = 0;
    /// for triple in TriGParser::new().parse_from_read(file.as_ref()) {
    ///     let triple = triple?;
    ///     if triple.predicate == rdf_type && triple.object == schema_person.into() {
    ///         count += 1;
    ///     }
    /// }
    /// assert_eq!(2, count);
    /// # Result::<_,Box<dyn std::error::Error>>::Ok(())
    /// ```
    pub fn parse_from_read<R: Read>(&self, read: R) -> FromReadTriGReader<R> {
        FromReadTriGReader {
            inner: self.parse().parser.parse_from_read(read),
        }
    }

    /// Allows to parse a TriG file by using a low-level API.
    ///
    /// Count the number of people:
    /// ```
    /// use oxrdf::NamedNodeRef;
    /// use oxttl::{TriGParser, ParseError};
    ///
    /// let file: [&[u8]; 5] = [b"@base <http://example.com/>",
    ///     b". @prefix schema: <http://schema.org/> .",
    ///     b"<foo> a schema:Person",
    ///     b" ; schema:name \"Foo\" . <bar>",
    ///     b" a schema:Person ; schema:name \"Bar\" ."
    /// ];
    ///
    /// let rdf_type = NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
    /// let schema_person = NamedNodeRef::new("http://schema.org/Person")?;
    /// let mut count = 0;
    /// let mut parser = TriGParser::new().parse();
    /// let mut file_chunks = file.iter();
    /// while !parser.is_end() {
    ///     // We feed more data to the parser
    ///     if let Some(chunk) = file_chunks.next() {
    ///         parser.extend_from_slice(chunk);    
    ///     } else {
    ///         parser.end(); // It's finished
    ///     }
    ///     // We read as many triples from the parser as possible
    ///     while let Some(triple) = parser.read_next() {
    ///         let triple = triple?;
    ///         if triple.predicate == rdf_type && triple.object == schema_person.into() {
    ///             count += 1;
    ///         }
    ///     }
    /// }
    /// assert_eq!(2, count);
    /// # Result::<_,Box<dyn std::error::Error>>::Ok(())
    /// ```
    pub fn parse(&self) -> LowLevelTriGReader {
        LowLevelTriGReader {
            parser: TriGRecognizer::new_parser(
                true,
                #[cfg(feature = "rdf-star")]
                self.with_quoted_triples,
                self.base.clone(),
                self.prefixes.clone(),
            ),
        }
    }
}

/// Parses a TriG file from a [`Read`](std::io::Read) implementation. Can be build using [`TriGParser::parse_from_read`].
///
/// Count the number of people:
/// ```
/// use oxrdf::NamedNodeRef;
/// use oxttl::{TriGParser, ParseError};
///
/// let file = b"@base <http://example.com/> .
/// @prefix schema: <http://schema.org/> .
/// <foo> a schema:Person ;
///     schema:name \"Foo\" .
/// <bar> a schema:Person ;
///     schema:name \"Bar\" .";
///
/// let rdf_type = NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
/// let schema_person = NamedNodeRef::new("http://schema.org/Person")?;
/// let mut count = 0;
/// for triple in TriGParser::new().parse_from_read(file.as_ref()) {
///     let triple = triple?;
///     if triple.predicate == rdf_type && triple.object == schema_person.into() {
///         count += 1;
///     }
/// }
/// assert_eq!(2, count);
/// # Result::<_,Box<dyn std::error::Error>>::Ok(())
/// ```
pub struct FromReadTriGReader<R: Read> {
    inner: FromReadIterator<R, TriGRecognizer>,
}

impl<R: Read> Iterator for FromReadTriGReader<R> {
    type Item = Result<Quad, ParseOrIoError>;

    fn next(&mut self) -> Option<Result<Quad, ParseOrIoError>> {
        self.inner.next()
    }
}

/// Parses a TriG file by using a low-level API. Can be build using [`TriGParser::parse`].
///
/// Count the number of people:
/// ```
/// use oxrdf::NamedNodeRef;
/// use oxttl::{TriGParser, ParseError};
///
/// let file: [&[u8]; 5] = [b"@base <http://example.com/>",
///     b". @prefix schema: <http://schema.org/> .",
///     b"<foo> a schema:Person",
///     b" ; schema:name \"Foo\" . <bar>",
///     b" a schema:Person ; schema:name \"Bar\" ."
/// ];
///
/// let rdf_type = NamedNodeRef::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
/// let schema_person = NamedNodeRef::new("http://schema.org/Person")?;
/// let mut count = 0;
/// let mut parser = TriGParser::new().parse();
/// let mut file_chunks = file.iter();
/// while !parser.is_end() {
///     // We feed more data to the parser
///     if let Some(chunk) = file_chunks.next() {
///         parser.extend_from_slice(chunk);    
///     } else {
///         parser.end(); // It's finished
///     }
///     // We read as many triples from the parser as possible
///     while let Some(triple) = parser.read_next() {
///         let triple = triple?;
///         if triple.predicate == rdf_type && triple.object == schema_person.into() {
///             count += 1;
///         }
///     }
/// }
/// assert_eq!(2, count);
/// # Result::<_,Box<dyn std::error::Error>>::Ok(())
/// ```
pub struct LowLevelTriGReader {
    parser: Parser<TriGRecognizer>,
}

impl LowLevelTriGReader {
    /// Adds some extra bytes to the parser. Should be called when [`read_next`](Self::read_next) returns [`None`] and there is still unread data.
    pub fn extend_from_slice(&mut self, other: &[u8]) {
        self.parser.extend_from_slice(other)
    }

    /// Tell the parser that the file is finished.
    ///
    /// This triggers the parsing of the final bytes and might lead [`read_next`](Self::read_next) to return some extra values.
    pub fn end(&mut self) {
        self.parser.end()
    }

    /// Returns if the parsing is finished i.e. [`end`](Self::end) has been called and [`read_next`](Self::read_next) is always going to return `None`.
    pub fn is_end(&self) -> bool {
        self.parser.is_end()
    }

    /// Attempt to parse a new quad from the already provided data.
    ///
    /// Returns [`None`] if the parsing is finished or more data is required.
    /// If it is the case more data should be fed using [`extend_from_slice`](Self::extend_from_slice).
    pub fn read_next(&mut self) -> Option<Result<Quad, ParseError>> {
        self.parser.read_next()
    }
}
