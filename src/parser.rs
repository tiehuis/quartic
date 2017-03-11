//! Handles parsing of various free-form inputs.
//
// TODO: Needs to be determined how we want to handle errors here. Likely start
// by just re-exporting those from `combine`.

use chord::*;

use combine::{Stream, ParseResult, Parser};
use combine::{many, one_of};

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

#[cfg(test)]
mod tests {
    use super::*;
    use combine::{ParseError, State, parser};
    use combine::primitives::SourcePosition;
    use combine::primitives::Error::{Expected, Unexpected};
    use combine::primitives::Info::{Borrowed, Token};

    #[test]
    fn parse_note_no_accidentals() {
        let result = parser(note).parse("A");
        assert_eq!(result, Ok((Note::new(NoteClass::A, 0), "")));

        let result = parser(note).parse("G");
        assert_eq!(result, Ok((Note::new(NoteClass::G, 0), "")));
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

        assert_eq!(result, Ok((Note::new(NoteClass::B, -1), "asd")));
    }

    #[test]
    fn parse_note_accidentals() {
        let result = parser(note).parse("A#");
        assert_eq!(result, Ok((Note::new(NoteClass::A, 1), "")));

        let result = parser(note).parse("F#bb");
        assert_eq!(result, Ok((Note::new(NoteClass::F, -1), "")));
    }
}
