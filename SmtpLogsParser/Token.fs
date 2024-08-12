module SmtpLogsParser.Token

type Token =
    | IPAddress of string
    | Url of string
    | Hostname of string
    | Mailbox of string
    | SmtpCode of string
    | Word of string
    | SpaceBar of string
    | PunctuationMark of string
    | Identifier of string
