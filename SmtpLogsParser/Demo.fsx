#r "nuget: FParsec"
#load "Parsers/DomainParser.fs"
#load "Parsers/EmailParser.fs"
#load "Parsers/IPParser.fs"

open FParsec
open SmtpLogsParser.Parsers.EmailParser
open SmtpLogsParser.Parsers.IPParser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
          
let multipleEmailsParser =
    sepBy emailParser spaces1 .>> eof |>> String.concat " -+- "
    
test ipParser "192.43.142.84";
test emailParser "surrenderplease@gmail.com!#@";
test multipleEmailsParser "surrend.erplease@gmail.com sur333333333renderplease@gmail.com surrenderplease@gmail.com"