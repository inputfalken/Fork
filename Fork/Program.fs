open BlackFox.ColoredPrintf
open FOrchestrator
open FOrchestrator.Process
open System
open System.Diagnostics
open System.IO

type ProcessStartInfoProvider = FSharp.Data.JsonProvider<"""
[
    {
        "workingDirectory": "path",
        "fileName": "dotnet",
        "arguments": "run",
        "alias": "name"
    }
]
""">

[<EntryPoint>]
let main argv =
    let stop p = try (p, TimeSpan.FromSeconds 30.) ||> Process.RecursiveKill |> ignore with :? System.InvalidOperationException as x -> ()
    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let start p = p |> (fun x -> Process.Run x Console.WriteLine) |> Async.Start

    let rec session input processes processFactory =
        let stopProcesses p = p |> List.iter (fun x -> x.Process |> stop)
        match input() with
        | "restart" ->
            processes |> stopProcesses
            let processes = processFactory()
            processes |> List.iter start
            (input, processes, processFactory) |||> session
        | "stop" -> processes |> stopProcesses; (input, [], processFactory) |||> session
        | "list" -> processes |> List.iter (fun x -> printfn "%s (%s) = %s %s %s" x.Alias (isAlive x.Process) x.Process.StartInfo.FileName x.Process.StartInfo.Arguments x.Process.StartInfo.WorkingDirectory); (input, processes, processFactory) |||> session
        | "killDotnet" ->
            Process.GetProcessesByName("dotnet")
            |> Array.filter (fun x -> x.Id <> Process.GetCurrentProcess().Id)
            |> Array.map stop
            |> ignore
            (input, [], processFactory) |||> session
        | "exit" -> processes
        | _ -> (input, processes, processFactory) |||> session

    let arguments = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".fork.json")
                  |> File.ReadAllText
                  |> ProcessStartInfoProvider.Parse
                  |> Seq.map (fun x -> {
                          WorkingDirectory = x.WorkingDirectory
                          FileName = x.FileName
                          Arguments = x.Arguments
                          Alias = x.Alias
                      })
                  |> Seq.toList

    let processFactory() = arguments |> List.map (fun x -> Process.Create x (fun y -> ColoredPrintf.colorprintfn "%s $yellow[->] %s" x.Alias y.Data) Console.WriteLine)
    let processes = processFactory()
    processes |> List.iter start

    // BUG This event is not valid of the session spawns new processes. This should be regisered and deregistered inside the session
    AppDomain.CurrentDomain.ProcessExit
    |> Event.merge (Console.CancelKeyPress |> Event.map (fun _ -> EventArgs.Empty))
    |> Event.add (fun _ -> processes |> List.iter (fun x -> stop x.Process))
    session Console.ReadLine processes processFactory |> List.iter (fun x -> x.Alias |> printfn "Stopping '%s'..."; x.Process |> stop)
    0
