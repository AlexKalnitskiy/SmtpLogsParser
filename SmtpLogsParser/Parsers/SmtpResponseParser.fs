module SmtpLogsParser.Parsers.SmtpResponseParser

open DomainParser
open IPParser
open EmailParser
open SmtpCodeParser
open FParsec
open UrlParser
open SmtpLogsParser
open Token

let smtpResponseParser =
    let hostnameParser = domainParser |>> Hostname
    let ipAddressParser = ipParser |>> IPAddress
    let mailboxParser = emailParser |>> Mailbox
    let smtpCodeParser: Parser<Token, unit> = smtpCodeParser |>> SmtpCode
    let smtpExtendedCodeParser = smtpExtendedCodeParser |>> SmtpCode

    let spaceBarParser: Parser<Token, unit> =
        many1Satisfy (fun x -> x = ' ') |>> SpaceBar

    let punctuationMark: Parser<Token, unit> =
        anyOf "\"-,.!?:;<[()]>" |>> _.ToString() |>> PunctuationMark

    let word: Parser<Token, unit> =
        many1Satisfy (fun x -> isLetter x || x = ''') .>> notFollowedBy digit |>> Word

    let urlParser: Parser<Token, unit> = urlParser |>> Url

    let identifier: Parser<Token, unit> = sepBy1 (many1Satisfy (fun c -> isLetter c || isDigit c)) (anyOf "-_") |>> fun x -> Identifier (String.concat "" x)

    let anyTokenParser =
        choice
            [
              attempt smtpCodeParser
              attempt smtpExtendedCodeParser
              attempt urlParser
              attempt mailboxParser
              attempt ipAddressParser
              attempt hostnameParser
              attempt punctuationMark
              attempt spaceBarParser
              attempt word
              identifier ]

    many1 anyTokenParser .>> eof
