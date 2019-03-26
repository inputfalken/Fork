module Application
open System
open System.Diagnostics
open Fork.Process
open BlackFox.ColoredPrintf
open System.IO

// TODO the json should have the option to tell if the process should be run in a separate window.
type ProcessStartInfoProvider = FSharp.Data.JsonProvider<"""
[
   {
      "alias":"group",
      "tasks":[
         {
            "workingDirectory":"path",
            "fileName":"dotnet",
            "arguments":"run",
            "alias":"name",
            "useSeperateWindow":"true"
         },
         {
            "workingDirectory":"path",
            "fileName":"dotnet",
            "arguments":"run",
            "alias":"name",
            "useSeperateWindow":""
         }
      ]
   }
]
""">

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
[<EntryPoint>]
let main argv =
    let rec session (context : Context) =
        let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
        let start p = p |> (fun x -> Fork.Process.Run x context.OutputFunction) |> Async.Start
        let stop (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                      ||> Fork.Process.RecursiveKill
                                      |> List.filter (fun x -> x.ExitCode <> 0)
                                      |> List.map (fun x -> sprintf "Warning '%s' did not exit properly '%s' (%i)" p.Alias x.Output x.ExitCode)
                                      |> List.iter context.OutputFunction
                                  with :? System.InvalidOperationException as x -> ()
        context.ExitResolver |> Option.iter (fun x -> x.Event.RemoveHandler x.Handler)
        let stopProcesses pList = (pList : FProcess list) |> List.iter (fun x -> sprintf "Stopping '%s'..." x.Alias |> context.OutputFunction; x |> stop)
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
                context |> session
            else
                context.Processes |> stopProcesses
                { InputFunction = context.InputFunction; OutputFunction = context.OutputFunction; Processes = []; ProcessSpawner = context.ProcessSpawner; ExitResolver = exitResolver } |> session

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
                                x |> List.iter start
                                x |> List.append context.Processes
                            | None -> context.Processes
            {

              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              Processes = processes
              ProcessSpawner = context.ProcessSpawner
              ExitResolver = exitResolver
            } |> session


        let restartSession() =
            if context.Processes.IsEmpty then
                context.OutputFunction "There's no active procceses."
                context |> session
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
                                        stop x
                                    )
                                    let newProcesses = context.ProcessSpawner()
                                                     |> List.collect (fun x -> x.Processes)
                                                     |> List.filter (fun x -> x.Alias = input)

                                    let res = (context.Processes |> List.filter (fun x -> x.Alias <> input))
                                    newProcesses |> List.iter start
                                    res |> List.append newProcesses
                                | _ -> context.Processes
                {
                  InputFunction = context.InputFunction
                  OutputFunction = context.OutputFunction
                  Processes = processes
                  ProcessSpawner = context.ProcessSpawner
                  ExitResolver = exitResolver
                } |> session

        match context.InputFunction() with
        | "restart" -> restartSession()
        | "start" -> startSession()
        | "stop" -> stopSession()
        | "list" -> context.Processes |>
                    List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    |> List.iter context.OutputFunction
                    context |> session
        | "exit" -> context.Processes
        | _ -> context |> session



    let arguments = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".fork.json")
                  |> File.ReadAllText
                  |> ProcessStartInfoProvider.Parse
                  |> Seq.map (fun x -> {
                      Alias = x.Alias
                      Tasks = x.Tasks |> Array.map (fun x -> {
                          WorkingDirectory = x.WorkingDirectory
                          FileName = x.FileName
                          Arguments = x.Arguments
                          Alias = x.Alias
                          UseSeperateWindow = match x.UseSeperateWindow with
                                              | Some x -> x
                                              | None -> false

                      }) |> Array.toList
                  })
                  |> Seq.toList

    let processWithStdout x = Fork.Process.Create x (fun y -> ColoredPrintf.colorprintfn "%s $yellow[->] %s" x.Alias y.Data) Console.WriteLine

    let processFactory() = arguments |> List.map (fun x -> { Processes = x.Tasks |> List.map processWithStdout; Alias = x.Alias })

    {
        InputFunction = Console.ReadLine
        OutputFunction = Console.WriteLine
        Processes = []
        ProcessSpawner = processFactory
        ExitResolver = None
    }
    |> session
    |> ignore
    0
