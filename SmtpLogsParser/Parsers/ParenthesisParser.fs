module SmtpLogsParser.Parsers.ParenthesisParser

open FParsec
let parenthesisParser pp: Parser<'a, unit> =
    let parens p = between (pchar '(') (pchar ')') p
    let brackets p = between (pchar '[') (pchar ']') p
    let angles p = between (pchar '<') (pchar '>') p

    choice [parens pp; brackets pp; angles pp]