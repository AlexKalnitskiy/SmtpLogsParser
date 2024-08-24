module SmtpLogsParser.Parsers.DateTimeParser

open System
open FParsec
open Microsoft.FSharp.Core


let date =
    stringsSepBy1 (many1 digit |>> fun x -> String(Array.ofList x)) (anyOf "-." |>> _.ToString())

let time = stringsSepBy1 (many1 digit |>> fun x -> String(Array.ofList x)) (anyOf ":." |>> _.ToString()) .>>. opt (pchar 'Z') |>> fun (x, y) ->
    let a = match y with Some a -> a.ToString() | None -> String.Empty;
    $"{x}{a}" 
let dateTimeAsStringParser: Parser<string, unit> = pipe3 date (opt (anyOf "T ")) time (fun d p t -> $"{d}{match p with Some a -> a.ToString() | None -> String.Empty}{t}")

let parseDateTime (input: string) =
    try
        let dateTime = DateTime.Parse(input)
        preturn dateTime
    with
    | :? FormatException -> fail "Invalid date format"
let dateTimeParser = dateTimeAsStringParser >>= parseDateTime