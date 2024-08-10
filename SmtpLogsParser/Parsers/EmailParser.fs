module SmtpLogsParser.Parsers.EmailParser

open DomainParser
open FParsec
let emailParser : Parser<string, unit> =
    
    let localPartParser =
        let allowedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'*+/=?^_`{|}~-."
        many1Satisfy allowedChars.Contains
    
    pipe3 localPartParser (pchar '@') domainParser
        (fun user _ dom -> user + "@" + dom)