use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    #[error("invalid char \"{}\"", .0)]
    InvalidChar(char),
}
