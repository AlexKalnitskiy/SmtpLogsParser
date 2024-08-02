namespace SmtpLogsParser

open FParsec

module Parser = 

    let number: Parser<string, unit> = many1Satisfy isDigit

    // Парсер точки
    let dot = pchar '.'

    // Парсер IP-адреса
    let ipAddress = 
        number .>> dot .>>. number .>> dot .>>. number .>> dot .>>. number
        |>> fun ((((a, b), c), d)) -> sprintf "%s.%s.%s.%s" a b c d

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
        
    test pfloat "1.25"