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
   Processes : FProcess list
   ProcessSpawner : unit -> StartInfo list
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
    let exitResolver = if context.Processes.IsEmpty then
                            None
                        else
                             {
                                 Event = System.AppDomain.CurrentDomain.ProcessExit
                                         |> Event.map (fun x -> Choice1Of2 x)
                                         |> Event.merge (Console.CancelKeyPress |> Event.map (fun x -> Choice2Of2 x))
                                 Handler = Handler<Choice<EventArgs, ConsoleCancelEventArgs>>(fun _ arg -> sprintf "%A" arg |> context.OutputFunction; context.Processes |> stopProcesses)
                              } |> (fun x -> x.Event.AddHandler x.Handler; Some x)
    let stopSession() =
        if context.Processes.IsEmpty then
            context.OutputFunction "There's no active procceses."
            context |> start
        else
            context.Processes |> stopProcesses
            { InputFunction = context.InputFunction; OutputFunction = context.OutputFunction; Processes = []; ProcessSpawner = context.ProcessSpawner; ExitResolver = exitResolver } |> start

    let startSession() =
        context.OutputFunction "Input your alias."
        let input = context.InputFunction()
        let isNotInRunningProcesses = context.Processes
                                   |> List.filter (fun x -> x.Alias = input)
                                   |> Option.Some
                                   |> Option.filter (fun x -> x.IsEmpty)

        let newProcess = context.ProcessSpawner()
                       |> Option.Some
                       |> Option.map (List.collect (fun x -> x.Processes))
                       |> Option.map (List.filter (fun x -> x.Alias = input))
                       |> Option.filter (fun x -> not x.IsEmpty)
                       |> Option.map (fun x -> x.[0])
                       |> Option.map (fun x -> [ x ])

        let processes = match newProcess with
                        | Some x ->
                            x |> List.iter startProcess
                            x |> List.append context.Processes
                        | None -> context.Processes
        {

          InputFunction = context.InputFunction
          OutputFunction = context.OutputFunction
          Processes = processes
          ProcessSpawner = context.ProcessSpawner
          ExitResolver = exitResolver
        } |> start


    let restartSession() =
        if context.Processes.IsEmpty then
            context.OutputFunction "There's no active procceses."
            context |> start
        else
            context.OutputFunction "Input your alias."
            let input = context.InputFunction()

            let searchResult = context.Processes
                                       |> List.filter (fun x -> x.Alias = input)
                                       |> Option.Some
                                       |> Option.filter (fun x -> not x.IsEmpty)

            let processes = match searchResult with
                            | Some x ->
                                x |> List.iter (fun x ->
                                    // If the argumets supplied for creating the process is exposed
                                    // I do not need to spawn process objects.
                                    stopProcess x
                                )
                                let newProcesses = context.ProcessSpawner()
                                                 |> List.collect (fun x -> x.Processes)
                                                 |> List.filter (fun x -> x.Alias = input)

                                let res = (context.Processes |> List.filter (fun x -> x.Alias <> input))
                                newProcesses |> List.iter startProcess
                                res |> List.append newProcesses
                            | _ -> context.Processes
            {
              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              Processes = processes
              ProcessSpawner = context.ProcessSpawner
              ExitResolver = exitResolver
            } |> start

    match context.InputFunction() with
    | "restart" -> restartSession()
    | "start" -> startSession()
    | "stop" -> stopSession()
    | "list" -> context.Processes |>
                List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                |> List.iter context.OutputFunction
                context |> start
    | "exit" -> context.Processes
    | _ -> context |> start
