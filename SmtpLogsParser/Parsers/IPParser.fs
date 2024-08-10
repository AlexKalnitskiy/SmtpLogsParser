module SmtpLogsParser.Parsers.IPParser

open FParsec

let ipParser: Parser<string, unit> =
    let oneToThreeDigits = puint8
    pipe4 (oneToThreeDigits .>> pchar '.') (oneToThreeDigits .>> pchar '.') (oneToThreeDigits .>> pchar '.') oneToThreeDigits (fun a b c d -> String.concat "." [a.ToString(); b.ToString(); c.ToString(); d.ToString()])