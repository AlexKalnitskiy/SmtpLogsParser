module SmtpLogsParser.Parsers.SmtpCodeParser

open FParsec

let smtpCodeParser: Parser<string, unit> = manyMinMaxSatisfy 3 3 isDigit .>> followedBy (anyOf ['-'; ' '])

let smtpExtendedCodeParser: Parser<string, unit> =  pipe3 (digit .>> pchar '.') (digit .>> pchar '.') (manyMinMaxSatisfy 1 3 isDigit) (fun a b c -> String.concat "." [ a.ToString(); b.ToString(); c ])