//! Handles parsing of various free-form inputs.
//
// TODO: Needs to be determined how we want to handle errors here. Likely start
// by just re-exporting those from `combine`.

use chord::*;

use combine::{Stream, ParseResult, Parser};
use combine::{parser, many, one_of, optional, token};

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

    (root, third)
        .map(|(root, third)| {
            Chord::new(
                root,
                ChordStructure::new()
                    .insert(third)
                    .insert((PitchClass::N5, 0))
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
            Note::new(NoteClass::A, 1),
            ChordStructure::new()
                .insert_many(&[(N3, -1), (N5, 0)])
        );

        assert_eq!(result, Ok((expected, "")));
    }
}
