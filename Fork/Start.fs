module internal Command.Start
open ProcessHandler
open State

// TODO: create proper union type
let internal search (processes : StartInfo list) (input : string) = Result.Ok processes
                                                                  |> Result.map (List.filter (fun x -> x.Alias = input))
                                                                  |> Result.bind (fun x -> if x.IsEmpty then Result.Error processes else Result.Ok x)
                                                                  |> Result.map (List.collect (fun x -> x.Processes))
                                                                  |> Result.mapError (List.collect (fun x -> x.Processes))
                                                                  |> Result.mapError (List.filter (fun x -> x.Alias = input))
let internal Exec input context exitResolver startProcess =

    let processes = match (search context.Processes input) with
                    | Result.Ok x -> x
                    | Result.Error x -> x
                    |> List.map (fun x -> x.Arguments)
                    |> List.map context.ProcessFactory
                    |> List.append context.ActiveProcesses

    processes |> List.iter startProcess

    {
      InputFunction = context.InputFunction
      OutputFunction = context.OutputFunction
      ActiveProcesses = processes
      Processes = context.Processes
      ExitResolver = exitResolver
      ProcessFactory = context.ProcessFactory
    }
