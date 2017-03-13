//! Defines types used to construct representations of chords and notes.
//!
//! These can be generated manually, but often one would want to use the
//! `parser` module into order to generate these from a string.
//!
//! ```
//! use quartic::chord::{Chord, ChordStructure, Note, NoteClass, PitchClass};
//!
//! /// Manual construction of a A#Maj13(#5)(#11)
//! let root = Note::new(NoteClass::A, 1);
//! let structure =
//!     ChordStructure::new()
//!         .insert_many(&[
//!             (PitchClass::N3, 0),
//!             (PitchClass::N5, 1),
//!             (PitchClass::N7, 1),
//!             (PitchClass::N9, 0),
//!             (PitchClass::N11, 1),
//!             (PitchClass::N13, 0),
//!         ]);
//!
//! let a = Chord::new(root, structure);
//! ```

/// A single note without accidentals.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NoteClass {
    A, B, C, D, E, F, G
}

impl NoteClass {
    pub fn from_char(input: char) -> Option<NoteClass> {
        use self::NoteClass::*;

        match input {
            'A' => Some(A),
            'B' => Some(B),
            'C' => Some(C),
            'D' => Some(D),
            'E' => Some(E),
            'F' => Some(F),
            'G' => Some(G),
            _   => None
        }
    }
}

/// Relative pitch compared to some base note as part of a chord.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PitchClass {
    N1, N2, N3, N4, N5, N6, N7, N9, N11, N13
}

/// The total number of `PitchClass` elements.
pub const PITCH_CLASS_COUNT: usize = 10;

impl PitchClass {
    /// Allows `PitchClass` to be used as an indexable element.
    pub fn index(&self) -> usize {
        use self::PitchClass::*;

        match *self {
            N1  => 0,
            N2  => 1,
            N3  => 2,
            N4  => 3,
            N5  => 4,
            N6  => 5,
            N7  => 6,
            N9  => 7,
            N11 => 8,
            N13 => 9,
        }
    }

    /// Returns a slice over the extended chord intervals for the specified
    /// pitch class.
    ///
    /// For example, a `C11` chord implicitly includes the lower extended
    /// intervals of the `7`th and `9`th within its representation. Passing
    /// `PitchClass::n11` would return a slice with `7`, `9` and `11` values.
    pub fn extended_intervals(&self) -> &'static [ChordComponent] {
        use self::PitchClass::*;

        static CLASSES: [ChordComponent; 4] = [
            (N7, 0), (N9, 0), (N11, 0), (N13, 0),
        ];

        match *self {
            N7  => &CLASSES[..1],
            N9  => &CLASSES[..2],
            N11 => &CLASSES[..3],
            N13 => &CLASSES[..4],
            _   => &[]
        }
    }
}

/// Represents an alteration of a base `NoteClass`.
///
/// This is analagous to accidentals, with positive values representing
/// repeated sharps and negative values representing repeated flats.
pub type PitchOffset = i8;

/// A single note which may have applied accidentals.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Note {
    /// The base note
    pub root: NoteClass,

    /// The resulting shift from accidentals
    pub offset: PitchOffset
}

impl Note {
    /// Construct and return a new `Note`.
    pub fn new(root: NoteClass, offset: PitchOffset) -> Note {
        Note { root, offset }
    }
}

/// A relative note within a chord by its intervallic representation.
///
/// For example, a (PitchClass::n7, 1) would represent a sharpened seventh,
/// as found in a major seventh chord.
pub type ChordComponent = (PitchClass, PitchOffset);

/// Represents the intervallic structure of a chord.
///
/// This is relative to a root note so a transposition is very cheap.
#[derive(Clone, Debug, PartialEq)]
pub struct ChordStructure([Option<PitchOffset>; PITCH_CLASS_COUNT]);

impl ChordStructure {
    /// Construct and return a new `ChordStructure`.
    ///
    /// This will always have the root note (`PitchClass::n1`) by default.
    pub fn new() -> ChordStructure {
        let mut classes = [None; PITCH_CLASS_COUNT];
        classes[PitchClass::N1.index()] = Some(0);

        ChordStructure(classes)
    }

    /// Construct a `ChordStructure` from a single component.
    ///
    /// This is provided to simplify merging alterations into a core chord and
    /// may be removed at some point.
    pub fn from_component(component: ChordComponent) -> ChordStructure {
        let mut classes = [None; PITCH_CLASS_COUNT];
        classes[component.0.index()] = Some(component.1);

        ChordStructure(classes)
    }

    /// Insert a single `ChordComponent` into this `ChordStructure`.
    ///
    /// This will overwrite any existing component if the interval was already
    /// set.
    pub fn insert(mut self, component: ChordComponent) -> ChordStructure {
        self.0[component.0.index()] = Some(component.1);
        self
    }

    /// Insert a slice of `ChordComponent` values into the `ChordStructure`.
    ///
    /// This will overwrite any existing components for each of the intervals
    /// which are already present in the input slice.
    pub fn insert_many(mut self, components: &[ChordComponent]) -> ChordStructure {
        for &component in components {
            self.0[component.0.index()] = Some(component.1);
        }
        self
    }

    /// Merge two `ChordStructure`'s together with preference for elements
    /// within the `other` structure.
    pub fn merge(mut self, other: &ChordStructure) -> ChordStructure {
        for i in 0..PITCH_CLASS_COUNT {
            if other.0[i].is_some() {
                self.0[i] = other.0[i];
            }
        }
        self
    }
}

/// A single simple chord comprised of many notes.
///
/// The chord representation used internally is based on tertian harmony.
#[derive(Clone, Debug, PartialEq)]
pub struct Chord {
    /// The base note
    pub root: Note,

    /// The relative intervallic structure of this chord
    pub structure: ChordStructure
}

impl Chord {
    /// Construct and return a new `Chord`.
    pub fn new(root: Note, structure: ChordStructure) -> Chord {
        Chord { root, structure }
    }
}
