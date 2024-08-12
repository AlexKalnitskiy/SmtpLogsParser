module SmtpLogsParser.Parsers.DomainParser

open FParsec

    
let domainParser: Parser<string, unit> =
    let firstChar = letter <|> digit
    let label = 
        let subsequentChar = letter <|> digit <|> pchar '-'
        pipe2 firstChar (many subsequentChar) (fun first rest -> first :: rest |> System.String.Concat)
        
    let notLastDot = pchar '.' .>>? followedBy firstChar
      
    let parser = label .>>. many1 (notLastDot >>. label) |>> fun (first, rest) -> first :: rest |> String.concat "."
    
    parser