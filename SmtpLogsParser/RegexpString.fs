[<RequireQualifiedAccess>]
module SmtpLogsParser.RegexpString

open Token

let escapeRegexCharacters (input: string) : string =
    let specialChars =
        [| '.'; '^'; '$'; '*'; '+'; '?'; '('; ')'; '['; ']'; '{'; '}'; '\\'; '|'; '/' |]

    let escapeChar (c: char) = if Array.contains c specialChars then "\\" + string c else string c

    input |> Seq.map escapeChar |> String.concat ""

let rec render tokens =
    let renderToken token =
        match token with
        | IPAddress _ -> "\S+"
        | Url _ -> "\S+"
        | Hostname _ -> "\S+"
        | Mailbox _ -> "\S+"
        | SmtpCode _ -> "\S+"
        | Word w -> w
        | SpaceBar s -> s
        | PunctuationMark p -> p |> escapeRegexCharacters
        | Identifier _ -> "\S+"

    tokens |> Seq.map renderToken |> String.concat ""
