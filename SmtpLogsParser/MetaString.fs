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
        | SpaceBar s -> s
        | PunctuationMark p -> p
        | Identifier _ -> "{{IDENTIFIER}}"
     
     tokens |> Seq.map renderToken |> String.concat ""

    
