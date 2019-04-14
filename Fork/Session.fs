module Fork.Session
open FSharp.Data.Runtime.StructuralInference
open Fork
open Fork
open Fork.InputAnalyzer
open ProcessHandler
open State
open System.Diagnostics
open System

let rec internal start (context : Context) =
    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let startProcess p = p |> (fun x -> ProcessHandler.Run x context.OutputFunction) |> Async.Start
    let stopProcess (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                         ||> ProcessHandler.RecursiveKill
                                         |> List.filter (fun x -> x.ExitCode <> 0)
                                         |> List.map (fun x -> sprintf "Warning '%s' did not exit properly '%s' (%i)" p.Alias x.Output x.ExitCode)
                                         |> List.iter context.OutputFunction
                                     with :? System.InvalidOperationException as x -> ()
    context.ExitResolver |> Option.iter (fun x -> x.Event.RemoveHandler x.Handler)
    let stopProcesses pList = (pList : FProcess list) |> List.iter (fun x -> sprintf "Stopping '%s'..." x.Alias |> context.OutputFunction; x |> stopProcess)
    // This can cause a stackoverflow exception if it runs for to long...
    let exitResolver = if context.ActiveProcesses.IsEmpty then
                            None
                        else
                             {
                                 Event = System.AppDomain.CurrentDomain.ProcessExit
                                         |> Event.map (fun x -> Choice1Of2 x)
                                         |> Event.merge (Console.CancelKeyPress |> Event.map (fun x -> Choice2Of2 x))
                                 Handler = Handler<Choice<EventArgs, ConsoleCancelEventArgs>>(fun _ arg -> sprintf "%A" arg |> context.OutputFunction; context.ActiveProcesses |> stopProcesses)
                              } |> (fun x -> x.Event.AddHandler x.Handler; Some x)

    match context.InputFunction() with
    | Result.Ok command ->
        match command with
        | InputAnalyzer.Command.CommandEnum x ->
            match x with
            | CommandEnum.Exit -> context.ActiveProcesses
            | CommandEnum.List ->
                if
                    context.ActiveProcesses.IsEmpty then context.OutputFunction "There's no active processes."; context |> start
                else
                    context.ActiveProcesses |> List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    |> List.iter context.OutputFunction
                    context |> start
            | _ -> raise (NotImplementedException())
        | InputAnalyzer.AliasCommand x ->
            match x.Command with
            | AliasCommandEnum.Stop ->
                 Command.Stop.Exec x.Alias context exitResolver stopProcess |> start
            | AliasCommandEnum.Start ->
                Command.Start.Exec x.Alias context exitResolver startProcess |> start
            | AliasCommandEnum.Restart ->
                Command.Restart.Exec x.Alias context exitResolver startProcess stopProcess |> start
            | _ -> raise (NotImplementedException())
    | Result.Error x ->
        context.OutputFunction x; context |> start
