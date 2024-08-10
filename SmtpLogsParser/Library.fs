namespace SmtpLogsParser

open FParsec

module Parser = 

    // Парсер для email
    let emailParser : Parser<string, unit> =
        let username = many1SatisfyL (fun c -> isLetter c || isDigit c || c = '.' || c = '-' || c = '_') "username"
        let domain = many1SatisfyL (fun c -> isLetter c || isDigit c || c = '-' || c = '.') "domain"
        let tld = many1SatisfyL isLetter "tld"
        
        pipe3 username (pchar '@') (domain .>> pchar '.' .>>. tld)
            (fun user _ (dom, tld) -> user + "@" + dom + "." + tld)

    // Функция для замены всех email-ов в строке
    let replaceEmails (input: string) =
        let rec loop acc pos =
            match runParserOnSubstring emailParser () "" input pos (input.Length - pos) with
            | Success(email, _, nextPos) ->
                let nextIndex = int nextPos.Index
                let pre = input.Substring(pos, nextIndex - pos - email.Length)
                loop (acc + pre + "{Mailbox}") nextIndex
            | Failure _ ->
                acc + input.Substring(pos)
        
        loop "" 0

    // Тестовая строка
    let input = "550- Message was not accepted -- invalid mailbox. Local mailbox 89263491107@mail.ru is unavailable: account is disabled"

    // Вывод результата
    let result = replaceEmails input
    printfn "%s" result
