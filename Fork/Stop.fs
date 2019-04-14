module internal Command.Stop

open State

let internal Exec input context exitResolver stopProcess =
    if context.ActiveProcesses.IsEmpty then
        context.OutputFunction "There's no active procceses."
        context
    else
        let aliasGroup = Option.Some context.Processes
                       |> Option.map (List.filter (fun x -> x.Alias = input))
                       |> Option.filter (fun x -> not x.IsEmpty)
                       |> Option.map (List.collect (fun x -> x.Processes))

        if aliasGroup.IsSome then
            let aliasGroupProcesses = aliasGroup.Value
            aliasGroupProcesses |> List.iter stopProcess

            {
              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              ActiveProcesses = context.ActiveProcesses |> List.filter (fun x -> not (aliasGroupProcesses |> List.map (fun x -> x.Alias) |> List.contains x.Alias))
              Processes = context.Processes
              ExitResolver = exitResolver
              ProcessFactory = context.ProcessFactory
            }
        else
            let searchResult = Result.Ok context.ActiveProcesses
                             |> Result.map (List.filter (fun x -> x.Alias = input))
                             |> Result.bind (fun x -> if x.IsEmpty then Result.Error(sprintf "The alias '%s' is not running." input) else Result.Ok x)
                             |> Result.map (fun x -> x.[0])

            let processes = match searchResult with
                            | Result.Ok x ->
                                stopProcess x
                                context.ActiveProcesses |> List.filter (fun x -> x.Alias <> input)
                            | Result.Error x ->
                                context.OutputFunction x
                                context.ActiveProcesses

            {
                InputFunction = context.InputFunction
                OutputFunction = context.OutputFunction
                ActiveProcesses = processes
                Processes = context.Processes
                ExitResolver = exitResolver
                ProcessFactory = context.ProcessFactory
            }

