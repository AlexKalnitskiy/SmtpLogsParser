module SmtpLogsParser.Parsers.SmtpCodeParser

open FParsec

let smtpCodeParser: Parser<string, unit> =
    let smtpCode = manyMinMaxSatisfy 3 3 isDigit

    let emssCode =
        pipe3 (digit .>> pchar '.') (digit .>> pchar '.') (manyMinMaxSatisfy 1 3 isDigit) (fun a b c -> String.concat "." [ a.ToString(); b.ToString(); c ])

    let p = anyOf " -"

    pipe3 smtpCode (skipMany p) (opt emssCode) (fun smtp _ emss ->
        smtp
        + match emss with
          | Some e -> "-" + e
          | None -> "")
