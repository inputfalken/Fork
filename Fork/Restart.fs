module internal Command.Restart
open State
open ProcessHandler

let internal search (processes : StartInfo list) (activeProcesses : FProcess list) (input : string) =
    let aliasGroupSearch = processes |> List.filter (fun x -> x.Alias = input)
    if aliasGroupSearch.IsEmpty
        then
            activeProcesses
            |> List.filter (fun x -> x.Alias = input)
            |> (fun x -> if x.IsEmpty then Option.None else Option.Some x.[0])
            |> (fun x -> (x, input) |> SearchResult.Alias)
        else
            activeProcesses
            |> List.filter (fun x -> (aliasGroupSearch
                                          |> List.collect (fun x -> x.Processes)
                                          |> List.map (fun x -> x.Alias)
                                          |> List.contains x.Alias
                                          )
            )
            |> (fun x -> (x, input)) |> SearchResult.AliasGroup

let internal Exec input context exitResolver startProcess stopProcess =
    if context.ActiveProcesses.IsEmpty then
        context.OutputFunction "There's no active procceses."
        context
    else
        let restart x = x |> stopProcess; x.Arguments |> startProcess ; x
        let processes = match search context.Processes context.ActiveProcesses input with
                        | Alias(x, y) -> match x with
                                         | Some x -> x |> (fun x -> [ restart x ])
                                         | None -> context.ActiveProcesses
                        | AliasGroup(x, y) ->
                            x |> List.map (fun x -> restart x)
                        |> (fun processes -> context.ActiveProcesses
                                             |> List.filter (fun x -> not (processes |> List.exists (fun y -> y.Alias = x.Alias)))
                                             |> List.append processes
                           )

        {
          InputFunction = context.InputFunction
          OutputFunction = context.OutputFunction
          ActiveProcesses = processes
          Processes = context.Processes
          ExitResolver = exitResolver
          ProcessFactory = context.ProcessFactory
        }
