open System
open Fork.Process
open System.Diagnostics
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

[<EntryPoint>]
let main argv =
    
    let isAlive (p : Process) = try p.Responding |> ignore; "running" with :? System.InvalidOperationException as x -> "stopped"
    let start p = p |> (fun x -> Fork.Process.Run x Console.WriteLine) |> Async.Start
    let stop (p : FProcess) = try (p.Process, TimeSpan.FromSeconds 30.)
                                  ||> Fork.Process.RecursiveKill
                                  |> List.filter (fun x -> x.ExitCode <> 0)
                                  |> List.iter (fun x -> printfn "Warning '%s' did not exit properly '%s' (%i)" p.Alias x.Output x.ExitCode)
                              with :? System.InvalidOperationException as x -> ()

    let rec session input processes processFactory =
        // TODO Close down all `processess` before exiting the session.
        let stopProcesses pList = (pList : FProcess list) |> List.iter (fun x -> printfn "Stopping '%s'..." x.Alias; x |> stop)
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
            |> Array.map Fork.Process.RecursiveKill
            |> ignore
            (input, [], processFactory) |||> session
        | "exit" -> processes
        | _ -> (input, processes, processFactory) |||> session

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

    session Console.ReadLine processes processFactory |> List.iter (fun x -> x.Alias |> printfn "Stopping '%s'..."; x |> stop)
    0
