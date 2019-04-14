module internal Command.Restart
open State

let internal Exec input context exitResolver startProcess stopProcess =
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
            aliasGroupProcesses
            |> List.map (fun x -> x.Arguments)
            |> List.map context.ProcessFactory
            |> List.iter startProcess

            {
              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              ActiveProcesses = context.ActiveProcesses |> List.filter (fun x -> aliasGroupProcesses |> List.map (fun x -> x.Alias) |> List.contains x.Alias)
              Processes = context.Processes
              ExitResolver = exitResolver
              ProcessFactory = context.ProcessFactory
            } 
         else
            let searchResult = context.ActiveProcesses
                             |> Result.Ok
                             |> Result.map (List.filter (fun x -> x.Alias = input))
                             |> Result.bind (fun x -> if x.IsEmpty then Result.Error(sprintf "Could not find a process with the input '%s'." input) else Result.Ok x)
                             |> Result.bind (fun x -> if x.Length > 1 then Result.Error(sprintf "More than one process was matched with the input '%s'." input) else Result.Ok x)
                             |> Result.map (fun x -> x.[0])

            let processes = match searchResult with
                            | Result.Ok x ->
                                stopProcess x
                                x.Arguments |> context.ProcessFactory |> startProcess
                                context.ActiveProcesses
                                |> List.filter (fun x -> x.Alias <> input)
                                |> List.append [ x ]
                            | Result.Error x ->
                                sprintf "ERROR: %s." x |> context.OutputFunction
                                context.ActiveProcesses

            {
              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              ActiveProcesses = processes
              Processes = context.Processes
              ExitResolver = exitResolver
              ProcessFactory = context.ProcessFactory
            }
