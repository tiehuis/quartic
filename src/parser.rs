//! Handles parsing of various free-form inputs.
//
// TODO: Needs to be determined how we want to handle errors here. Likely start
// by just re-exporting those from `combine`.

#![allow(dead_code)]

use chord::*;

use combine::{Stream, ParseResult, Parser};
use combine::{between, choice, parser, many, one_of, optional, token, try, chainl1};
use combine::char::{string, spaces};

/// Parses a single accidental.
///
/// ```text
/// Accidental : 'b' | '#' | '♭' | '♯' | '𝄪'
///            ;
/// ```
///
/// Note: The last symbol present is the double-sharp. Since it is quite
/// uncommon it may not be rendered correctly with many fonts.
fn accidental<I>(input: I) -> ParseResult<PitchOffset, I>
    where I: Stream<Item=char>
{
    one_of("b#♭♯𝄪".chars())
        .map(|x| match x {
            '#' | '♯' =>  1,
            'b' | '♭' => -1,
            '𝄪'       =>  2,
            _ => unreachable!()
        })
        .parse_stream(input)
}

/// Parses a root note plus its accidentals.
///
/// An example of a note is `A#bb`. Note that accidentals are reduced as much
/// as they can be **without** changing the base note given.
///
/// ```text
/// Note : [A-G] Accidental*
///      ;
/// ```
fn note<I>(input: I) -> ParseResult<Note, I>
    where I: Stream<Item=char>
{
    let root_note =
        one_of("ABCDEFG".chars())
            .map(NoteClass::from_char)
            .expected("Note: [A-G]");

    // TODO: This could be more efficient with an iterator.
    let offset =
        many(parser(accidental))
            .map(|x: Vec<_>| x.iter().sum())
            .expected("Accidental: [b#♭♯]*");

    (root_note, offset)
        .map(|(root, offset)| Note::new(root.unwrap(), offset))
        .parse_stream(input)
}

/// Parses a standard chord using the default rules.
///
/// This will recognize standard chords based on a third/seventh interval with
/// a possible extended interval present.
///
/// An example of a chord this parser reconizes is `F#mMaj7`.
///
/// ```text
/// ThirdQuality : 'min' | 'mi' | 'm' | '-'
///              ;
///
/// SeventhQuality : 'Maj' | 'Ma' | 'M' | 'Δ'
///                ;
///
/// ExtendedInterval : '7' | '9' | '11' | '13'
///                  ;
///
/// ChordStandard : ThirdQuality? (SeventhQuality ExtendedInterval)?
///               ;
/// ```
fn chord_standard<I>(input: I) -> ParseResult<ChordStructure, I>
    where I: Stream<Item=char>
{
    let third =
        optional(choice([
                try(string("min")), try(string("mi")),
                try(string("m")), try(string("-")),
            ]))
            .map(|q| match q {
                Some("min") | Some("mi") | Some("m") | Some("-") => {
                    ChordStructure::from_component((PitchClass::N3, -1))
                        .insert((PitchClass::N5, 0))
                }

                Some(_) => unreachable!(),

                None => {
                    ChordStructure::from_component((PitchClass::N3, 0))
                        .insert((PitchClass::N5, 0))
                }
            });

    let seventh =
        optional(choice([
                try(string("Maj")), try(string("Ma")),
                try(string("M")), try(string("Δ"))
            ]))
            .map(|q| match q {
                Some("Maj") | Some("Ma") | Some("M") | Some("Δ") => {
                    (PitchClass::N7, 1)
                }

                _ => (PitchClass::N7, 0)
            });

    let interval =
        choice([
            try(string("7")), try(string("9")),
            try(string("11")), try(string("13"))
        ])
        .map(|q| match q {
            "7"  => PitchClass::N7,
            "9"  => PitchClass::N9,
            "11" => PitchClass::N11,
            "13" => PitchClass::N13,
            _ => unreachable!()
        }.extended_intervals());

    let extended =
        optional(seventh.and(interval))
            .map(|q| match q {
                Some((q, i)) => {
                    ChordStructure::new()
                        .insert_many(i)
                        .insert(q)
                }

                None => ChordStructure::new()
            });

    (third, extended)
        .map(|(third, extended)| {
            ChordStructure::new()
                .merge(&third)
                .merge(&extended)
        })
        .parse_stream(input)
}

/// Parses an augmented or diminished chord including its direct extensions
/// (i.e. `Adim7`).
///
/// ```text
/// ExtendedQuality : '7'
///                 ;
///
/// Diminished : 'dim' | '°'
///            ;
///
/// Augmented : 'aug' | '+'
///           ;
///
/// ChordAugDim : (Augmented | Diminished) ExtendedQuality?
///             ;
/// ```
fn chord_aug_or_dim<I>(input: I) -> ParseResult<ChordStructure, I>
    where I: Stream<Item=char>
{
    let mut aug_dim =
        choice([
            try(string("dim")), try(string("°")),
            try(string("aug")), try(string("+"))
        ])
        .and(optional(token('7')))
        .map(|(q, e)| match q {
            "dim" | "°" => {
                let m = ChordStructure::from_component((PitchClass::N3, -1))
                            .insert((PitchClass::N5, -1));

                if e.is_some() {
                    m.insert((PitchClass::N7, -1))
                } else {
                    m
                }
            }

            "aug" | "+" => {
                let m = ChordStructure::from_component((PitchClass::N3, 0))
                            .insert((PitchClass::N5, 1));

                if e.is_some() {
                    m.insert((PitchClass::N7, 0))
                } else {
                    m
                }
            }

            _ => unreachable!(),
        });

    aug_dim.parse_stream(input)
}

/// Parses special chords which do not use standard thirds/sevenths.
///
/// An example of a chord this parser reconizes is `F5`.
///
/// ```text
/// FifthChord : '5'
///            ;
///
/// ChordSpecial : FifthChord | ChordAugDim
///              ;
/// ```
fn chord_special<I>(input: I) -> ParseResult<ChordStructure, I>
    where I: Stream<Item=char>
{
    let chord_fifth =
        token('5')
            .map(|_| ChordStructure::new().insert((PitchClass::N5, 0)))
            .expected("Chord: 5");

    chord_fifth.or(parser(chord_aug_or_dim)).parse_stream(input)
}

/// Parses a set of chord alterations that may appear at the end of a chord.
///
/// An example of a set of alterations is final enclosed group in the
/// chord, `C7(#5,b9)`.
///
/// ```text
/// Alteration : Accidental ('4' | '5' | '6' | '9' | '11' | '13')
///            ;
///
/// Alterations : '(' (Alteration ',')* ')'
///             ;
/// ```
fn chord_alterations<I>(input: I) -> ParseResult<ChordStructure, I>
    where I: Stream<Item=char>
{
    let altered_interval =
        choice([
            try(string("5")), try(string("6")), try(string("9")),
            try(string("11")), try(string("13"))
        ])
        .map(|q| match q {
            "4"  => PitchClass::N4,
            "5"  => PitchClass::N5,
            "6"  => PitchClass::N6,
            "9"  => PitchClass::N9,
            "11" => PitchClass::N11,
            "13" => PitchClass::N13,
            _ => unreachable!()
        });

    let altered_offset = parser(accidental);

    let alteration =
        altered_offset.and(altered_interval)
            .map(|(o, i)| ChordStructure::from_component((i, o)));

    let chain_op =
        (optional(spaces()), token(','), optional(spaces()))
            .map(|_| |l: ChordStructure, r: ChordStructure| l.merge(&r));

    optional(between(token('(').and(optional(spaces())), token(')'),
        chainl1(alteration, chain_op)
    ))
    .map(|q| match q {
        Some(inner) => inner,
        None => ChordStructure::new()
    })
    .parse_stream(input)
}

/// Parse a trailing slash chord extension.
///
/// And example of a slash extension this parses is `/A#`.
///
/// ```text
/// SlashExtension : '/' Note
///                ;
/// ```
fn slash_extension<I>(input: I) -> ParseResult<Note, I>
    where I: Stream<Item=char>
{
    (token('/'), parser(note))
        .map(|(_, note)| note)
        .parse_stream(input)
}

/// Recognizes an entire chord of any type.
///
/// ```text
/// Chord : Note (ChordSpecial | ChordStandard) ChordAlterations SlashExtension?
///       ;
/// ```
fn chord<I>(input: I) -> ParseResult<Chord, I>
    where I: Stream<Item=char>
{
    let chord = try(parser(chord_special)).or(try(parser(chord_standard)));

    (
        parser(note),
        chord,
        parser(chord_alterations),
        optional(parser(slash_extension))
    )
    .map(|(root, standard, alterations, slash)| {
        Chord {
            slash_root: slash,
            root: root,
            structure: ChordStructure::new()
                            .merge(&standard)
                            .merge(&alterations)
        }
    })
    .parse_stream(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use combine::{ParseError, State, parser};
    use combine::primitives::SourcePosition;
    use combine::primitives::Error::{Expected, Unexpected};
    use combine::primitives::Info::{Borrowed, Token};

    use chord::NoteClass::*;
    use chord::PitchClass::*;

    #[test]
    fn parse_note_no_accidentals() {
        let result = parser(note).parse("A");
        assert_eq!(result, Ok((Note::new(A, 0), "")));

        let result = parser(note).parse("G");
        assert_eq!(result, Ok((Note::new(G, 0), "")));
    }

    #[test]
    fn parse_note_unicode_accidentals() {
        let result = parser(note).parse("A♯#");
        assert_eq!(result, Ok((Note::new(A, 2), "")));

        let result = parser(note).parse("G♭");
        assert_eq!(result, Ok((Note::new(G, -1), "")));
    }

    #[test]
    fn parse_note_unicode_accidentals_double_sharp() {
        let result = parser(note).parse("F♯𝄪b");
        assert_eq!(result, Ok((Note::new(F, 2), "")));
    }

    #[test]
    fn parse_note_invalid_root() {
        let result = parser(note).parse(State::new("I"));
        let expected = Err(ParseError {
            position: SourcePosition { line: 1, column: 1 },
            errors: vec![Unexpected(Token('I')), Expected(Borrowed("Note: [A-G]"))]
        });

        assert_eq!(result, expected);
    }

    #[test]
    fn parse_note_trailing_junk() {
        let result = parser(note).parse("Bbasd");

        assert_eq!(result, Ok((Note::new(B, -1), "asd")));
    }

    #[test]
    fn parse_note_accidentals() {
        let result = parser(note).parse("A#");
        assert_eq!(result, Ok((Note::new(A, 1), "")));

        let result = parser(note).parse("F#bb");
        assert_eq!(result, Ok((Note::new(F, -1), "")));
    }

    #[test]
    fn parse_simple_chord() {
        let result = parser(chord).parse("A#");
        let expected = Chord::new(
            Note::new(A, 1),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 0)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_minor_chord() {
        let result = parser(chord).parse("A#m");
        let expected = Chord::new(
            Note::new(A, 1),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, 0)])
        );

        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("A#min");
        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("A#mi");
        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("A#-");
        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_seventh_chord() {
        let result = parser(chord).parse("A#Maj9");
        let expected = Chord::new(
            Note::new(A, 1),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 0), (N7, 1), (N9, 0)])
        );

        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("A#Δ9");
        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_minor_seventh_chord() {
        let result = parser(chord).parse("Cm7");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, 0), (N7, 0)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_dom_seventh_chord() {
        let result = parser(chord).parse("E13");
        let expected = Chord::new(
            Note::new(E, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 0), (N7, 0), (N9, 0), (N11, 0), (N13, 0)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_minor_major_seventh() {
        let result = parser(chord).parse("CmMaj7");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, 0), (N7, 1)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_alterations() {
        let result = parser(chord).parse("Cm(#5)");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, 1)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_alterations_multi_spaced() {
        let result = parser(chord).parse("CMaj7( b5,   #11 , b9)");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, -1), (N7, 1), (N9, -1), (N11, 1)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_alterations_multi() {
        let result = parser(chord).parse("CMaj9(b5,#11)");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, -1), (N7, 1), (N9, 0), (N11, 1)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_augmented_triad() {
        let result = parser(chord).parse("C+");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 1)])
        );

        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("Caug");
        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_diminished_triad() {
        let result = parser(chord).parse("Cdim");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, -1)])
        );

        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("C°");
        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_diminished_triad_extended() {
        let result = parser(chord).parse("Bbdim7");
        let expected = Chord::new(
            Note::new(B, -1),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, -1), (N7, -1)])
        );

        assert_eq!(result, Ok((expected.clone(), "")));

        let result = parser(chord).parse("Bb°7");
        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_augmented_extended() {
        let result = parser(chord).parse("C+7(#9)");
        let expected = Chord::new(
            Note::new(C, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 1), (N7, 0), (N9, 1)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_fifth_chord() {
        let result = parser(chord).parse("D5");
        let expected = Chord::new(
            Note::new(D, 0),
            ChordStructure::new()
                .insert_many(&[(N5, 0)])
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_slash_chord_simple() {
        let result = parser(chord).parse("A/C#");
        let expected = Chord::new_slash(
            Note::new(C, 1),
            Note::new(A, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 0)]),
        );

        assert_eq!(result, Ok((expected, "")));
    }

    #[test]
    fn parse_slash_chord_extended() {
        let result = parser(chord).parse("AMaj7(#5,#11)/C#");
        let expected = Chord::new_slash(
            Note::new(C, 1),
            Note::new(A, 0),
            ChordStructure::new()
                .insert_many(&[(N3, 0), (N5, 1), (N7, 1), (N11, 1)]),
        );

        assert_eq!(result, Ok((expected, "")));
    }
}
