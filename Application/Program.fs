open System
open System
open System
open Fork.Process
open System.Diagnostics
open BlackFox.ColoredPrintf
open Fork.Process
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

type ExitResolver = {
   Handler : Handler<Choice<EventArgs, ConsoleCancelEventArgs>>
   Event : IEvent<Choice<EventArgs, ConsoleCancelEventArgs>>
 }

type SessionState = {
   InputFunction : unit -> string
   Processes : FProcess list
   ProcessSpawner : unit -> FProcess list
   ExitResolver : ExitResolver option
 }
[<EntryPoint>]
let main argv =

    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let start p = p |> (fun x -> Fork.Process.Run x Console.WriteLine) |> Async.Start
    let stop (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                  ||> Fork.Process.RecursiveKill
                                  |> List.filter (fun x -> x.ExitCode <> 0)
                                  |> List.iter (fun x -> printfn "Warning '%s' did not exit properly '%s' (%i)" p.Alias x.Output x.ExitCode)
                              with :? System.InvalidOperationException as x -> ()

    let rec session (state : SessionState) =
        state.ExitResolver |> Option.iter (fun x -> x.Event.RemoveHandler x.Handler)
        let stopProcesses pList = (pList : FProcess list) |> List.iter (fun x -> printfn "Stopping '%s'..." x.Alias; x |> stop)
        // This can cause a stackoverflow exception if it runs for to long...
        let exitResolver = if state.Processes.IsEmpty then
                                None
                            else
                                 {
                                     Event = System.AppDomain.CurrentDomain.ProcessExit
                                             |> Event.map (fun x -> Choice1Of2 x)
                                             |> Event.merge (Console.CancelKeyPress |> Event.map (fun x -> Choice2Of2 x))
                                     Handler = Handler<Choice<EventArgs, ConsoleCancelEventArgs>>(fun _ arg -> printfn "%A" arg; state.Processes |> stopProcesses)
                                  } |> (fun x -> x.Event.AddHandler x.Handler; Some x)

        let nextSessionUnmodified() = { InputFunction = state.InputFunction; Processes = state.Processes; ProcessSpawner = state.ProcessSpawner; ExitResolver = exitResolver } |> session

        let nextSessionWithStoppedProcceses() =
            state.Processes |> stopProcesses
            { InputFunction = state.InputFunction; Processes = []; ProcessSpawner = state.ProcessSpawner; ExitResolver = exitResolver } |> session

        let nextSessionWithRestartedProccesses() =
            state.Processes |> stopProcesses
            let processes = state.ProcessSpawner()
            processes |> List.iter start
            {
              InputFunction = state.InputFunction
              Processes = processes
              ProcessSpawner = state.ProcessSpawner
              ExitResolver = exitResolver
            } |> session

        match state.InputFunction() with
        | "restart" -> nextSessionWithRestartedProccesses()
        | "stop" -> nextSessionWithStoppedProcceses()
        | "list" -> state.Processes |> List.iter (fun x -> printfn "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory)
                    nextSessionUnmodified()
        | "exit" -> state.Processes
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

    let processFactory() = arguments
                           |> List.map (fun x -> { Processes = x.Tasks |> Array.map processWithStdout; Alias = x.Alias })
                           |> List.collect (fun x -> x.Processes |> Array.toList)
    let processes = processFactory()
    processes |> List.iter start

    {
        InputFunction = Console.ReadLine
        Processes = processes
        ProcessSpawner = processFactory
        ExitResolver = None
    }
    |> session
    |> ignore
    0
