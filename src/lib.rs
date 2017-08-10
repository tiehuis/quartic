//! # quartic
//!
//! A crate providing a number of core music primitives and construction tools.
//!
//! The `Chord` module contains a performant representation of tertian chords
//! and provides functions for constructing and determining their constituent
//! notes.

#[macro_use]
extern crate combine;

pub mod chord;
mod parser;
