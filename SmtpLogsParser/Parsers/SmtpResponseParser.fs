module SmtpLogsParser.Parsers.SmtpResponseParser

open DomainParser
open IPParser
open EmailParser
open SmtpCodeParser
open FParsec
open SmtpLogsParser
open ParenthesisParser
open Token
  
let hostnameParser = domainParser |>> Hostname
let ipAddressParser = ipParser |>> IPAddress
let mailboxParser = emailParser |>> Mailbox
let smtpCodeParser: Parser<Token, unit> = smtpCodeParser |>> SmtpCode
let spaceBarParser: Parser<Token, unit> = many1Satisfy (fun x -> x = ' ') |>> SpaceBar
let punctuationMark: Parser<Token, unit> = anyOf ",.!?:;" |>> _.ToString() |>> PunctuationMark
let word: Parser<Token, unit> = many1Satisfy isLetter |>> Word
let volatility: Parser<Token, unit> = many1Satisfy (fun c -> c <> ' ') |>> Volatility

let anyTokenParser, anyTokenParserRef = createParserForwardedToRef<Token, unit>()
let manyOneParenthesis = parenthesisParser (many1 anyTokenParser) |>> fun x -> Parenthesis('[', x, ']')

do anyTokenParserRef.Value <- choice [
    attempt manyOneParenthesis
    attempt mailboxParser
    attempt ipAddressParser
    attempt hostnameParser
    attempt smtpCodeParser
    attempt word
    attempt punctuationMark
    attempt spaceBarParser
    volatility
]

let rec smtpResponseParser = many1 anyTokenParser .>> eof