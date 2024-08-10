#r "nuget: FParsec"

open FParsec

let domainParser =
    let identifierParser: Parser<string, unit> =
        many1SatisfyL (fun c -> isLetter c || isDigit c || c = '-') "identifier"
    let dotParser: Parser<unit, unit> =
        skipChar '.'
    let domainPartParser: Parser<string, unit> =
        identifierParser .>> dotParser
    let domainPartsParser: Parser<string list, unit> =
        many (attempt domainPartParser)
    domainPartParser .>>. domainPartsParser .>>. identifierParser |>> fun ((prefix, items), suffix) -> prefix :: items @ [suffix] |> String.concat "."

// Функция для тестирования парсера на входной строке
let testParser input =
    match run domainParser input with
    | Success(result, _, _) -> 
        printfn "Parse succeeded: %A" result
    | Failure(errorMsg, _, _) -> 
        printfn "Parse failed: %s" errorMsg

// Тестирование парсера
testParser "any.text.com"
testParser "sub.domain.example.com"
testParser "invalid-domain"
