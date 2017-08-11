# quartic
[![Build Status](https://travis-ci.org/tiehuis/quartic.svg?branch=master)](https://travis-ci.org/tiehuis/quartic)
[![Docs](https://docs.rs/quartic/badge.svg)](https://docs.rs/quartic)
[![Crate](https://img.shields.io/crates/v/quartic.svg)](https://crates.io/crates/quartic)

A library for dealing with music theory fundamentals.

Currently this only provides support for some chord representation and
simplistic parsing.

```
quartic = "0.1.0"
```

## Examples

```
use quartic::chord::{Chord, ChordStructure, Note, NoteClass, PitchClass};

/// Manual construction of a A#Maj13(#5,#11)
let root = Note::new(NoteClass::A, 1);
let structure = ChordStructure::new()
                    .insert_many(&[
                        (PitchClass::N3, 0),
                        (PitchClass::N5, 1),
                        (PitchClass::N7, 1),
                        (PitchClass::N9, 0),
                        (PitchClass::N11, 1),
                        (PitchClass::N13, 0),
                    ]);

let a = Chord::new(root, structure);
```
