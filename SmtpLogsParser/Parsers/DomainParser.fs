module SmtpLogsParser.Parsers.DomainParser

open FParsec
let domainParser =
    let identifierParser: Parser<string, unit> =
        many1SatisfyL (fun c -> isLetter c || isDigit c || c = '-') "identifier"
    let tld = many1SatisfyL isLetter "tld"
    let dotParser: Parser<unit, unit> =
        skipChar '.'
    let domainPartParser: Parser<string, unit> =
        identifierParser .>> dotParser
    let manyPartsParser: Parser<string list, unit> =
        many (attempt domainPartParser)
    domainPartParser .>>. manyPartsParser .>>. tld |>> fun ((prefix, items), suffix) -> prefix :: items @ [suffix] |> String.concat "."