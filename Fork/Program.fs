module Application
open Fork
open System
open Session
open Fork.Process
open BlackFox.ColoredPrintf
open FSharp.Data
open System.IO

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
    let processes = arguments |> List.map (fun x -> { Processes = x.Tasks |> List.map processWithStdout; Alias = x.Alias })

    let aliases = processes
                  |> List.collect (fun x -> x.Processes)
                  |> List.map (fun x -> x.Alias)
                  |> List.append (processes |> List.map (fun x -> x.Alias))
    let inputAnalyzer x = InputAnalyzer.ParseCommand x aliases

    {
        InputFunction = Console.ReadLine >> inputAnalyzer
        OutputFunction = Console.WriteLine
        ActiveProcesses = []
        ProcessFactory = processWithStdout
        Processes = processes
        ExitResolver = None
    }
    |> Session.start
    |> ignore
    0
