[<RequireQualifiedAccess>]
module SmtpLogsParser.MetaString

open Token

let rec render tokens =

    let renderToken token =
        match token with
        | IPAddress _ -> "{{IP}}"
        | Url _ -> "{{URL}}"
        | Hostname _ -> "{{HOSTNAME}}"
        | Mailbox _ -> "{{MAILBOX}}"
        | SmtpCode _ -> "{{SMTPCODE}}"
        | Word w -> w
        | DateTime _ -> "{{DATETIME}}"
        | SpaceBar s -> s
        | PunctuationMark p -> p
        | Identifier _ -> "{{IDENTIFIER}}"
        | Unknown _ -> "{{UNKNOWN}}"

    tokens |> Seq.map renderToken |> String.concat ""
