module Fork.Session
open Fork.Process
open System.Diagnostics
open System

type internal ExitResolver = {
   Handler : Handler<Choice<EventArgs, ConsoleCancelEventArgs>>
   Event : IEvent<Choice<EventArgs, ConsoleCancelEventArgs>>
 }

type internal Context = {
   InputFunction : unit -> string
   OutputFunction : string -> unit
   ActiveProcesses : FProcess list
   Processes : StartInfo list
   ProcessFactory : ProcessTask -> FProcess
   ExitResolver : ExitResolver option
 }

let rec internal start (context : Context) =
    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let startProcess p = p |> (fun x -> Fork.Process.Run x context.OutputFunction) |> Async.Start
    let stopProcess (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                         ||> Fork.Process.RecursiveKill
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
    let stopSession() =
        if context.ActiveProcesses.IsEmpty then
            context.OutputFunction "There's no active procceses."
            context |> start
        else
            context.ActiveProcesses |> stopProcesses
            {
                InputFunction = context.InputFunction
                OutputFunction = context.OutputFunction
                ActiveProcesses = []
                Processes = context.Processes
                ExitResolver = exitResolver
                ProcessFactory = context.ProcessFactory
            } |> start

    let startSession() =
        context.OutputFunction "Input your alias."
        let input = context.InputFunction()
        let searchResult = Result.Ok context.ActiveProcesses
                         |> Result.map (List.filter (fun x -> x.Alias = input))
                         |> Result.bind (fun x -> if x.IsEmpty then Result.Ok context.Processes else Result.Error(sprintf "There's already an process running under the alias '%s'." input))
                         |> Result.bind (fun x -> if x |> Seq.exists ( fun x -> x.Alias = input) then Result.Error "Group aliases are currently not supported." else Result.Ok x)
                         |> Result.map (List.collect (fun x -> x.Processes))
                         |> Result.map (List.filter (fun x -> x.Alias = input))
                         |> Result.bind (fun x -> if x.IsEmpty then Result.Error(sprintf "Could not find a process with the input '%s'." input) else Result.Ok x.[0])

        let processes = match searchResult with
                        | Ok x ->
                            x |> startProcess
                            context.ActiveProcesses |> List.append [ x ]
                        | Result.Error x ->
                            x |> sprintf "ERROR: %s" |> context.OutputFunction
                            context.ActiveProcesses
        {

          InputFunction = context.InputFunction
          OutputFunction = context.OutputFunction
          ActiveProcesses = processes
          Processes = context.Processes
          ExitResolver = exitResolver
          ProcessFactory = context.ProcessFactory
        } |> start


    let restartSession() =
        if context.ActiveProcesses.IsEmpty then
            context.OutputFunction "There's no active procceses."
            context |> start
        else
            context.OutputFunction "Input your alias."
            let input = context.InputFunction()

            let searchResult = context.ActiveProcesses
                             |> Result.Ok
                             |> Result.map (List.filter (fun x -> x.Alias = input))
                             |> Result.bind (fun x -> if x.IsEmpty then Result.Error(sprintf "Could not find a process with the input '%s'." input) else Result.Ok x)
                             |> Result.bind (fun x -> if x.Length > 1 then Result.Error(sprintf "More than one process was matched with the input '%s'." input) else Result.Ok x)
                             |> Result.map (fun x -> x.[0])

            let processes = match searchResult with
                            | Ok x ->
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
            } |> start

    match context.InputFunction() with
    | "restart" -> restartSession()
    | "start" -> startSession()
    | "stop" -> stopSession()
    | "list" -> if context.ActiveProcesses.IsEmpty then context.OutputFunction "There's no active processes."; context |> start
                else
                    context.ActiveProcesses |> List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    |> List.iter context.OutputFunction
                    context |> start
    | "exit" -> context.ActiveProcesses
    | _ -> context |> start
