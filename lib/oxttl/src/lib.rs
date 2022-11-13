mod lexer;
mod line_formats;
pub mod n3;
pub mod nquads;
pub mod ntriples;
mod terse;
mod toolkit;
pub mod trig;
pub mod turtle;

pub use crate::n3::N3Parser;
pub use crate::nquads::NQuadsParser;
pub use crate::ntriples::NTriplesParser;
pub use crate::toolkit::{ParseError, ParseOrIoError};
pub use crate::trig::TriGParser;
pub use crate::turtle::TurtleParser;

pub(crate) const MIN_BUFFER_SIZE: usize = 4096;
pub(crate) const MAX_BUFFER_SIZE: usize = 4096 * 4096;
