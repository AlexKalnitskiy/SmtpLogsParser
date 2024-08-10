[<RequireQualifiedAccess>]
module SmtpLogsParser.MetaString

open Token

 let rec render tokens =
     
     let renderToken token =
        match token with
        | Parenthesis (p1, b, p2) -> [ p1.ToString(); render b; p2.ToString() ] |> String.concat ""
        | IPAddress _ -> "{{IP}}"
        | Hostname _ -> "{{HOSTNAME}}"
        | Mailbox _ -> "{{MAILBOX}}"
        | SmtpCode _ -> "{{SMTPCODE}}"
        | Word w -> w
        | SpaceBar s -> s
        | PunctuationMark p -> p
        | Volatility _ -> "{{VOLATILITY}}"
     
     tokens |> Seq.map renderToken |> String.concat ""

    
