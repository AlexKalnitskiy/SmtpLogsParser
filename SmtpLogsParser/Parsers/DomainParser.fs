﻿module SmtpLogsParser.Parsers.DomainParser

open FParsec

    
let domainParser: Parser<string, unit> =
    let label = 
        let firstChar = letter <|> digit
        let subsequentChar = letter <|> digit <|> pchar '-'
        pipe2 firstChar (many subsequentChar) (fun first rest -> first :: rest |> System.String.Concat)
      
    let validTld e = not (Seq.contains '-' e)
           
    let result = sepBy1 label (pchar '.') >>= fun labels -> if List.length labels > 1 && (labels |> List.last) |> validTld then preturn labels else fail "hostname os not valid"
    
    result |>> String.concat "."