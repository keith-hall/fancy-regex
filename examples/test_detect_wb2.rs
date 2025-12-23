use regex_syntax::Parser;
use regex_syntax::hir::{Hir, HirKind, Look};

fn contains_word_boundary(hir: &Hir) -> bool {
    match hir.kind() {
        HirKind::Look(look) => matches!(
            look,
            Look::WordAscii | Look::WordUnicode |
            Look::WordAsciiNegate | Look::WordUnicodeNegate |
            Look::WordStartAscii | Look::WordEndAscii |
            Look::WordStartHalfAscii | Look::WordEndHalfAscii |
            Look::WordStartHalfUnicode | Look::WordEndHalfUnicode
        ),
        HirKind::Concat(subs) | HirKind::Alternation(subs) => {
            subs.iter().any(contains_word_boundary)
        }
        HirKind::Repetition(rep) => contains_word_boundary(&rep.sub),
        HirKind::Capture(cap) => contains_word_boundary(&cap.sub),
        _ => false,
    }
}

fn main() {
    let patterns = vec![
        r"\b(?:[A-Z][a-z]*|[a-z]+)",
        r"(?:[A-Z][a-z]*|[a-z]+)",
        r"a+b+",
        r"\Ba+",
    ];
    
    for pattern in patterns {
        let hir = Parser::new().parse(pattern).unwrap();
        let has_wb = contains_word_boundary(&hir);
        println!("Pattern '{}': contains word boundary = {}", pattern, has_wb);
    }
}
