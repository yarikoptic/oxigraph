mod lexer;
mod line_formats;
pub mod n3;
pub mod nquads;
pub mod ntriples;
mod toolkit;

pub use crate::n3::N3Parser;
pub use crate::nquads::NQuadsParser;
pub use crate::ntriples::NTriplesParser;
pub use crate::toolkit::{ParseError, ParseOrIoError};

pub(crate) const MIN_BUFFER_SIZE: usize = 4096;
pub(crate) const MAX_BUFFER_SIZE: usize = 4096 * 4096;
