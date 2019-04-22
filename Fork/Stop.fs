module internal Command.Stop
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

let internal Exec input context exitResolver stopProcess =
    if context.ActiveProcesses.IsEmpty then
        context.OutputFunction "There's no active procceses."
        context
    else
        let processes = match search context.Processes context.ActiveProcesses input with
                        | Alias(x, y) -> match x with
                                         | Some x ->
                                             x |> stopProcess
                                             [ x ]
                                         | None -> context.ActiveProcesses
                        | AliasGroup(x, y) ->
                            x |> List.iter stopProcess
                            x
        {

            InputFunction = context.InputFunction
            OutputFunction = context.OutputFunction
            ActiveProcesses = context.ActiveProcesses |> List.filter (fun x -> not (processes |> Seq.exists (fun y -> y.Alias = x.Alias)))
            Processes = context.Processes
            ExitResolver = exitResolver
            ProcessFactory = context.ProcessFactory
        }

