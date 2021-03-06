module Fork.Session
open FSharp.Data.Runtime.StructuralInference
open Fork
open InputAnalyzer
open ProcessHandler
open State
open System.Diagnostics
open Command
open System

let rec internal start (context : Context<'T>) =
    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let startProcess p = p
                         |> fun x -> x
                                     |> context.ProcessFactory
                                     |> fun x -> ProcessHandler.Run x context.OutputFunction, x
                         |> fun (x, y) -> Async.Start x; y
    let stopProcess (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                         ||> ProcessHandler.RecursiveKill
                                         |> List.filter (fun x -> x.ExitCode <> 0)
                                         |> List.map (fun x -> sprintf "Warning '%s' did not exit properly '%s' (%i)" p.Alias x.Output x.ExitCode)
                                         |> List.iter context.OutputFunction
                                     with :? System.InvalidOperationException as x -> ()
    context.ExitResolver |> Option.iter (fun x -> x.Event.RemoveHandler x.Handler)
    // This can cause a stackoverflow exception if it runs for to long...
    let exitResolver = if context.ActiveProcesses.IsEmpty then
                            None
                        else
                             let event = System.AppDomain.CurrentDomain.ProcessExit
                                         |> Event.map (fun x -> Choice1Of2 x)
                                         |> Event.merge (context.OnExit |> Event.map (fun x -> Choice2Of2 x))
                             {
                                 Event = event
                                 Handler = Handler<Choice<EventArgs, 'T>>(fun _ arg -> sprintf "%A" arg |> context.OutputFunction; context.ActiveProcesses |> List.iter stopProcess)
                             } |> (fun x -> x.Event.AddHandler x.Handler; Some x)

    match context.InputFunction() with
    | Result.Ok command ->
        match command with
        | CommandEnum x ->
            match x with
            | CommandEnum.Exit -> context.ActiveProcesses
            | CommandEnum.Alias ->
                let fold = Environment.NewLine + "  "
                context.Processes
                |> List.fold (fun x y -> (if x = String.Empty then x else x + Environment.NewLine) + y.Alias + (y.Processes |> List.fold (fun x y -> x + y.Alias + fold) (fold))) String.Empty
                |> context.OutputFunction
                |> (fun _ -> context.ActiveProcesses)
            | CommandEnum.List ->
                if context.ActiveProcesses.IsEmpty then context.OutputFunction "There's no active processes."
                else
                    context.ActiveProcesses |> List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    |> List.iter context.OutputFunction
                context.ActiveProcesses
            | _ -> raise (NotImplementedException())
        | AliasCommand x ->
            match x.Command with
            | AliasCommandEnum.Stop ->
                 if context.ActiveProcesses.IsEmpty then context.OutputFunction "There's no active procceses."; context.ActiveProcesses
                 else Stop.Exec x.Alias context.Processes context.ActiveProcesses stopProcess
            | AliasCommandEnum.Start -> Start.Exec x.Alias context.Processes context.ActiveProcesses startProcess
            | AliasCommandEnum.Restart -> Restart.Exec x.Alias context.Processes context.ActiveProcesses startProcess stopProcess
            | _ -> raise (NotImplementedException())
        |> (fun x ->
                    {
                      InputFunction = context.InputFunction
                      OutputFunction = context.OutputFunction
                      ActiveProcesses = x
                      Processes = context.Processes
                      ExitResolver = exitResolver
                      ProcessFactory = context.ProcessFactory
                      OnExit = context.OnExit
                    }
            )
        |> start
    | Result.Error x ->
        context.OutputFunction x; context |> start
