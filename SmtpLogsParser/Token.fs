module SmtpLogsParser.Token

open System

type Token =
    | IPAddress of string
    | Url of string
    | Hostname of string
    | Mailbox of string
    | SmtpCode of string
    | DateTime of DateTime
    | Word of string
    | SpaceBar of string
    | PunctuationMark of string
    | Identifier of string
    | Unknown of string
