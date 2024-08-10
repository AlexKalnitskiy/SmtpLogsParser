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
          
let OverlordParser =
    sepBy (choice [ attempt emailParser; ipParser ]) spaces1 .>> eof |>> String.concat " -+- "
    
test ipParser "192.43.142.84";
test emailParser "surrenderplease@gmail.com!#@";
test OverlordParser "surrend.erplease@gmail.com sur333333333renderplease@gmail.com 192.43.142.84 surrenderplease@gmail.com"