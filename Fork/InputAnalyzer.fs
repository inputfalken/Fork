module Fork.InputAnalyzer
open FParsec

type public CommandInput = { Command : string; Argument : string }

let public ParseCommand input aliases commands =
    let aliases = aliases
                 |> List.map pstring
                 |> List.reduce (<|>)
    let commands = commands
                 |> List.map pstring
                 |> List.reduce (<|>)

    let argumentParser = commands .>> spaces .>>. aliases |>> (fun (x, y) -> { Command = x; Argument = y })

    match run argumentParser input with
    | Success(x, y, z) -> Result.Ok x
    | Failure(x, y, z) -> Result.Error(sprintf "%A" x)
