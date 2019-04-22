module internal Command.Start
open ProcessHandler
open State

let internal search (processes : StartInfo list) (input : string) =
    let noAliasGroupSearch() =
        processes
        |> List.collect (fun x -> x.Processes)
        |> List.filter (fun x -> x.Alias = input)
        |> (fun x -> if x.IsEmpty then Option.None else Option.Some x.[0])

    let search = processes |> List.filter (fun x -> x.Alias = input)

    if search.IsEmpty
        then (noAliasGroupSearch(), input) |> SearchResult.Alias
        else (search |> List.collect (fun x -> x.Processes), input) |> SearchResult.AliasGroup

let internal Exec input context exitResolver startProcess =
    let processes = match search context.Processes input with
                    | Alias(x, y) -> match x with
                                     | Some x -> [ x ]
                                     | None -> []
                    | AliasGroup(x, y) -> x

    processes |> List.iter startProcess 

    {
      InputFunction = context.InputFunction
      OutputFunction = context.OutputFunction
      ActiveProcesses = context.ActiveProcesses @ processes 
      Processes = context.Processes
      ExitResolver = exitResolver
      ProcessFactory = context.ProcessFactory
    }
