module SmtpLogsParser.Parsers.UrlParser

open DomainParser
open FParsec

let urlParser : Parser<string, unit> =
    
    let schemeParser = pstring "https://"
        
    let portParser = opt (pchar ':' >>. pint32)
    
    let pathParser = opt (pchar '/' >>. manySatisfy (fun c -> c <> '?' && c <> '#'))
    
    let queryParamParser = sepBy (manySatisfy (fun c -> c <> '=' && c <> '&') .>> pchar '=' .>>. manySatisfy (fun c -> c <> '&')) (pchar '&')
    
    let queryParser = opt (pchar '?' >>. queryParamParser)
    
    let fragmentParser = opt (pchar '#' >>. manySatisfy (isNoneOf " "))
    
    let urlParser: Parser<string, unit> = pipe5 schemeParser domainParser portParser pathParser queryParser (fun scheme host port path query -> $"{scheme}{host}{port}{path}{query}") .>>. fragmentParser |>> fun (first, fragment) -> $"{first}{fragment}";
                              
    urlParser