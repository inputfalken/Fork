module Fork.InputAnalyzer
open System
open FParsec

type public AliasCommandEnum = | Restart = 0 | Stop = 1 | Start = 3
type public AliasCommand = { Command : AliasCommandEnum; Alias : string }
type public CommandEnum = | List = 0 | Exit = 1
type public Command =
    | AliasCommand of AliasCommand
    | CommandEnum of CommandEnum

let aliasCommands = [ "restart"; "stop"; "start" ]
let commands = [ "exit"; "list" ]
let (|InvariantEqual|_|) (str : string) arg =
  if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

let fromString<'a> (s : string) =
    Enum.Parse(typeof<'a>, s, true) :?> 'a

let public ParseCommand input aliases =

    let aliases = aliases
                 |> List.map pstringCI
                 |> List.reduce (<|>)

    let parserWithArgument = aliasCommands
                           |> List.map pstringCI
                           |> List.reduce (<|>)
                           .>> spaces
                           .>>.? aliases
                           .>> spaces
                           .>> notFollowedBy anyChar
                           |>> (fun (x, y) -> (fromString<AliasCommandEnum> x, y))
                           |>> (fun (x, y) -> { Command = x; Alias = y })
                           |>> Command.AliasCommand

    let parserWithoutArgument = commands
                                |> List.map pstringCI
                                |> List.reduce (<|>)
                                .>> spaces
                                .>> notFollowedBy anyChar
                                |>> (fun x -> fromString<CommandEnum> x)
                                |>> (Command.CommandEnum)

    match run (parserWithArgument <|> parserWithoutArgument) input with
    | Success(x, y, z) -> Result.Ok x
    | Failure(x, y, z) -> Result.Error(sprintf "%A" x)
