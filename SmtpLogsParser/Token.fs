module SmtpLogsParser.Token

type Token =
    | Parenthesis of char * Token list * char
    | IPAddress of string
    | Hostname of string
    | Mailbox of string
    | SmtpCode of string
    | Word of string
    | SpaceBar of string
    | PunctuationMark of string
    | Volatility of string