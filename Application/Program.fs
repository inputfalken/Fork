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

        let nextSessionUnmodified() = { InputFunction = context.InputFunction; OutputFunction = context.OutputFunction; Processes = context.Processes; ProcessSpawner = context.ProcessSpawner; ExitResolver = exitResolver } |> session

        let nextSessionWithStoppedProcceses() =
            context.Processes |> stopProcesses
            { InputFunction = context.InputFunction; OutputFunction = context.OutputFunction; Processes = []; ProcessSpawner = context.ProcessSpawner; ExitResolver = exitResolver } |> session

        let nextSessionWithRestartedProccesses() =
            context.OutputFunction "Input your alias."
            let input = context.InputFunction()

            let foundInExistingProcess = Option.Some context.Processes
                                        |> Option.map (fun x -> x |> List.filter (fun x -> x.Alias = input))
                                        |> Option.filter (fun x -> not x.IsEmpty)

            // This command should not have to spawn new processes!
            let spawns = context.ProcessSpawner()
            let foundGroupInSpawned = Option.Some spawns
                                      |> Option.map (fun x -> x |> List.filter (fun y -> y.Alias = input))
                                      |> Option.filter (fun x -> not x.IsEmpty)
                                      |> Option.map (fun x -> x |> List.collect (fun x -> x.Processes |> Array.toList))

            let searchResult = match foundInExistingProcess |> Option.orElse foundGroupInSpawned with
                                | Some x -> x
                                | None -> spawns
                                          |> List.collect (fun x -> x.Processes |> Array.toList)
                                          |> List.filter (fun x -> x.Alias = input)

            let processes = if searchResult.IsEmpty then
                                sprintf "Could not find an alias matching the name '%s'." input |> context.OutputFunction
                                context.Processes
                            else
                                searchResult |> List.iter (fun x -> stop x; start x)
                                let res = (context.Processes |> List.filter (fun x -> x.Alias <> input))
                                res |> List.append searchResult
            {
              InputFunction = context.InputFunction
              OutputFunction = context.OutputFunction
              Processes = processes
              ProcessSpawner = context.ProcessSpawner
              ExitResolver = exitResolver
            } |> session

        match context.InputFunction() with
        | "restart" -> nextSessionWithRestartedProccesses()
        | "stop" -> nextSessionWithStoppedProcceses()
        | "list" -> context.Processes |>
                    List.map (fun x -> sprintf "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    |> List.iter context.OutputFunction
                    nextSessionUnmodified()
        | "exit" -> context.Processes
        | _ -> nextSessionUnmodified()



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

                      })
                  })
                  |> Seq.toList

    let processWithStdout x = Fork.Process.Create x (fun y -> ColoredPrintf.colorprintfn "%s $yellow[->] %s" x.Alias y.Data) Console.WriteLine

    let processFactory() = arguments |> List.map (fun x -> { Processes = x.Tasks |> Array.map processWithStdout; Alias = x.Alias })

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
