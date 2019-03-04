open System
open System.Diagnostics
open System.IO

type Stdout = | Error | Output
type OutputType = { Data : string; Type : Stdout }

[<EntryPoint>]
let main argv =
    let createProcess (workingDirectory : string) (key : string) (fileName : string) (arguments : string) = async {
        let proc = Process.Start (new ProcessStartInfo(
                                         Arguments = arguments,
                                         CreateNoWindow = true,
                                         FileName = fileName,
                                         RedirectStandardError = true,
                                         RedirectStandardOutput = true,
                                         UseShellExecute = false,
                                         WorkingDirectory = workingDirectory
                                     )
                                )
        printfn "Starting process %s with id %i" proc.ProcessName proc.Id

        proc.Exited |> Event.add (fun x -> printfn "Process exited")
        proc.Disposed |> Event.add (fun x -> printfn "Process disposed")
        proc.OutputDataReceived
        |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Output })
        |> Event.merge (
               proc.ErrorDataReceived
               |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Error })
           )
        |> Event.add (fun x -> printfn "%A: %s %s" x.Type Environment.NewLine x.Data)
        proc.BeginErrorReadLine()
        proc.BeginOutputReadLine()
        return! Async.Sleep -1
    }



    let findTestApp path = Directory.GetCurrentDirectory
                           >> Directory.GetParent
                           >> (fun x -> x.GetDirectories())
                           >> Array.filter (fun x -> x.Name = path)
                           >> (fun x -> if x.Length = 1 then x.[0] else raise (new ArgumentException(x.Length |> sprintf "Expected array with %i length")))
                           >> (fun x -> x.FullName)
                           |> (fun x -> x())

    let dotnetRun = createProcess (findTestApp "TestWebApp") "MyApp" "dotnet" "run"
    dotnetRun |> Async.RunSynchronously

    0 // return an integer exit code


