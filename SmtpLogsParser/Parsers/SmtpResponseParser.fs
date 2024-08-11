module SmtpLogsParser.Parsers.SmtpResponseParser

open DomainParser
open IPParser
open EmailParser
open SmtpCodeParser
open FParsec
open SmtpLogsParser
open Token
  
let hostnameParser = domainParser |>> Hostname
let ipAddressParser = ipParser |>> IPAddress
let mailboxParser = emailParser |>> Mailbox
let smtpCodeParser: Parser<Token, unit> = smtpCodeParser |>> SmtpCode
let spaceBarParser: Parser<Token, unit> = many1Satisfy (fun x -> x = ' ') |>> SpaceBar
let punctuationMark: Parser<Token, unit> = anyOf ",.!?:;<[()]>" |>> _.ToString() |>> PunctuationMark
let word: Parser<Token, unit> = many1Satisfy isLetter .>> notFollowedBy digit |>> Word
let identifier: Parser<Token, unit> = many1Satisfy (fun c -> isLetter c || isDigit c || isAnyOf "-_" c) |>> Identifier
    
let anyTokenParser = choice [
    attempt mailboxParser
    attempt ipAddressParser
    attempt hostnameParser
    attempt smtpCodeParser
    attempt punctuationMark
    attempt spaceBarParser
    attempt word
    identifier
]

let rec smtpResponseParser = many1 anyTokenParser .>> eof