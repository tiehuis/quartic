//! Handles parsing of various free-form inputs.
//
// TODO: Needs to be determined how we want to handle errors here. Likely start
// by just re-exporting those from `combine`.

#![allow(dead_code)]

use chord::*;

use combine::{Stream, ParseResult, Parser};
use combine::{choice, parser, many, one_of, optional, token, try};
use combine::char::{string};

fn note<I>(input: I) -> ParseResult<Note, I>
    where I: Stream<Item=char>
{
    let root_note = one_of("ABCDEFG".chars())
                        .map(NoteClass::from_char)
                        .expected("Note: [A-G]");

    // TODO: This could be more efficient with an iterator.
    let offset = many(one_of("b#".chars()))
                    .map(|x: Vec<_>| {
                        x.iter().fold(0, |acc, x| match *x {
                            '#' => acc + 1,
                            'b' => acc - 1,
                             _  => acc
                        })
                    })
                    .expected("Accidental: [b#]*");

    (root_note, offset)
        .map(|(root, offset)| Note::new(root.unwrap(), offset))
        .parse_stream(input)
}

fn chord<I>(input: I) -> ParseResult<Chord, I>
    where I: Stream<Item=char>
{
    let root = parser(note);

    let third = optional(token('m'))
                    .map(|q| match q {
                        Some(_) => (PitchClass::N3, -1),
                        None => (PitchClass::N3, 0)
                    });

    let seventh = optional(choice([string("Maj")]))
                    .map(|q| match q {
                        Some("Maj") => (PitchClass::N7, 1),
                        _ => (PitchClass::N7, 0)
                    });

    let interval = choice([
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

    let extended = optional(seventh.and(interval))
                    .map(|q| match q {
                        Some((q, i)) => {
                            ChordStructure::new()
                                .insert_many(i)
                                .insert(q)
                        }

                        None => ChordStructure::new()
                    });

    (root, third, extended)
        .map(|(root, third, extended)| {
            Chord::new(
                root,
                ChordStructure::new()
                    .insert(third)
                    .insert((PitchClass::N5, 0))
                    .merge(&extended)
            )
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
}
