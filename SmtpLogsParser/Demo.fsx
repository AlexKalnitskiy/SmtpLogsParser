#r "nuget: FParsec"

open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
      
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

let emailParser : Parser<string, unit> =
    
    let localPartParser =
        let allowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'*+/=?^_`{|}~-"
        many1Satisfy allowedChars.Contains
    
    pipe3 localPartParser (pchar '@') domainParser
        (fun user _ dom -> user + "@" + dom)
    
let multipleEmailsParser =
    sepBy emailParser spaces1 |>> String.concat " -+- "
    
test emailParser "surrenderplease@gmail.com!#@";
test multipleEmailsParser "surrenderplease@gmail.com sur333333333renderplease@gmail.com surrenderplease@gmail.com"