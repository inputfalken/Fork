module Application
open Fork
open BlackFox.ColoredPrintf
open System
open State
open ProcessHandler
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
    let parsedJson = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".fork.json")
                  |> Result.Ok
                  |> Result.bind (fun x -> if File.Exists x then Result.Ok x else Result.Error(sprintf "Could not find file '%s'." x))
                  |> Result.map File.ReadAllText
                  |> Result.bind (fun x ->
                     try Result.Ok(ProcessStartInfoProvider.Parse x)
                     with e -> Result.Error e.Message)
                  |> Result.map (Seq.map (fun x -> {
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
                  }))
                  |> Result.map Seq.toList


    match parsedJson with
    | Ok tasks ->
                    let processWithStdout x = ProcessHandler.Create x (fun y -> ColoredPrintf.colorprintfn "%s $yellow[->] %s" x.Alias y.Data) Console.WriteLine
                    let processes = tasks |> List.map (fun x -> { Processes = x.Tasks |> List.map processWithStdout; Alias = x.Alias })

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
                        OnExit = Console.CancelKeyPress |> Event.map (fun x -> x)
                    }
                    |> Session.start
                    |> ignore
                    0
    | Result.Error x ->
       Console.WriteLine x
       1
