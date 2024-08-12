namespace SmtpLogsParser

open FParsec
open SmtpLogsParser.Parsers.SmtpResponseParser

type ParseResponse = { MatchingRegexp: string; MetaString: string }

type Parser =
    static member Parse(line: string) : Result<ParseResponse, string> =
        match run smtpResponseParser line with
        | Success(result, _, _) -> Result.Ok { MatchingRegexp = result |> RegexpString.render; MetaString = result |> MetaString.render }
        | Failure(errorMsg, _, _) -> Result.Error errorMsg
