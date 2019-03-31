module Fork.InputAnalyzer
open FParsec
open Process

type public CommandInput = { Command : char list; Argument : char list }

let public ParseInput input (processes : FProcess list) =
    let argumentParser = many1 letter .>> spaces .>>. many1 letter |>> (fun (x, y) -> { Command = x; Argument = y })

    match run argumentParser input with
    | Success(x, y, z) -> printfn "%A" x
    | Failure(x, y, z) -> printf "%A" x
    ()
