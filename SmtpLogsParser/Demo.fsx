#r "nuget: FParsec"
#load "Parsers/DomainParser.fs"
#load "Parsers/EmailParser.fs"
#load "Parsers/IPParser.fs"

open FParsec
open SmtpLogsParser.Parsers.DomainParser
open SmtpLogsParser.Parsers.EmailParser
open SmtpLogsParser.Parsers.IPParser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

type Token =
    | IPAddress of string
    | Hostname of string
    | Mailbox of string
    | SpaceBar of string
    
let hostnameParser = domainParser |>> Hostname
let ipAddressParser = ipParser |>> IPAddress
let mailboxParser = emailParser |>> Mailbox
let spaceBarParser: Parser<Token, unit> = many1Satisfy (fun x -> x = ' ') |>> SpaceBar

let anyTokenParser = choice
                         [
                             attempt spaceBarParser
                             attempt mailboxParser
                             attempt hostnameParser
                             ipAddressParser
                         ]
let OverLordParser = many1 anyTokenParser .>> eof |>> Seq.map (_.ToString()) |>> String.concat " "
    
test ipParser "192.43.142.84";
test emailParser "surrenderplease@gmail.com!#@";
test OverLordParser "surrend.erplease@gmail.com sur333333333renderplease@gmail.com 192.43.142.84 surrenderplease@gmail.com"