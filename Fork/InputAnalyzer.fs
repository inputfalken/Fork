module Fork.InputAnalyzer
open FParsec
open Fork.Process

let public ParseInput input (processes : FProcess list) =
    let argumentParser = many1 letter .>> spaces .>>. many1 letter

    match run argumentParser input with
    | Success((command, alias), y, z) -> printfn "Command: '%A', Alias: '%A'" command alias
    | Failure(x, y, z) -> printf "%A" x

    ()
