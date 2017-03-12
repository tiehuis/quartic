A library for dealing with music theory fundamentals.

Currently this only provides support for some chord representation and
simplistic parsing.

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
