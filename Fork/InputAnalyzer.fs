module Fork.InputAnalyzer
open FParsec
open Microsoft.FSharp.Reflection

type public CommandUnion = | List | Exit
type public AliasCommandUnion = | Restart | Stop | Start
type public AliasCommand = { Command : AliasCommandUnion; Alias : string }
type public Command =
    | AliasCommand of AliasCommand
    | CommandUnion of CommandUnion
    
let aliasCommands = [ "restart"; "stop"; "start" ]
let commands = [ "exit"; "list" ]

// BUG this is case sensitive
let fromString<'a> (s : string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
    | _ -> None
    
let public ParseCommand input aliases =

    let aliases = aliases
                 |> List.map pstringCI
                 |> List.reduce (<|>)

    let parserWithArgument = aliasCommands
                           |> List.map pstringCI
                           |> List.reduce (<|>)
                           .>> spaces
                           .>>.? aliases
                           |>> (fun (x, y) -> (fromString<AliasCommandUnion> x, y))
                           |>> (fun (x, y) -> { Command = x.Value; Alias = y })
                           |>> Command.AliasCommand

    let parserWithoutArgument = commands
                                |> List.map pstringCI
                                |> List.reduce (<|>)
                                |>> (fun x -> fromString<CommandUnion> x)
                                |>> Option.map (Command.CommandUnion)
                                |>> (fun x -> x.Value)

    match run (parserWithArgument <|> parserWithoutArgument) input with
    | Success(x, y, z) -> Result.Ok x
    | Failure(x, y, z) -> Result.Error(sprintf "%A" x)
