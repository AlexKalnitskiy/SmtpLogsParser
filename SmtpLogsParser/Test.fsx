open System

let bind switchFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> switchFn success
    | Error failure -> Error failure
    
let map f aResult =
    match aResult with
    | Ok success -> Ok (f success)
    | Error failure -> Error failure
    
let mapError f aResult =
    match aResult with
    | Ok success -> Ok success
    | Error failure -> Error (f failure)
    
let result10 = Error "cant calc 10"

type Error1 = string * string

type Error =
    | OrderNotCreated of string * int * DateTime
    | OrderValidationFailed of string

let createOrder userData: Result<int, Error1> = Error ("str1", "str2")
let validateOrder order = if order = 10 then Ok ("Valid", 10) else Error (OrderValidationFailed "ValidationError")



let handleError error = match error with
                            | OrderNotCreated (a, b, c) -> "obrabotali"
                            | OrderValidationFailed e -> "obrablil"
                            
let tee f x =
    f x
    x