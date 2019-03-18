module internal FOrchestrator.Process
open System.Diagnostics
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices

type ProcessResult = { ExitCode : int; Output : string }
type ProcessExit = { Code : int; Time : DateTime }
type Stdout = | Error | Output
type OutputType = { Data : string; Type : Stdout }
type FProcess = { Process : Process; Alias : string }
type StartInfo = { WorkingDirectory : string; FileName : string; Arguments : string; Alias : string }

// Taken from https://github.com/aspnet/Extensions/blob/ffb7c20fb22a31ac31d3a836a8455655867e8e16/shared/Microsoft.Extensions.Process.Sources/ProcessHelper.cs
let internal RecursiveKill (proc : Process) (timeout : TimeSpan) =
    let runProcessAndWaitForExit fileName arguments (timeout : TimeSpan) =
        let proc = new Process(StartInfo = new ProcessStartInfo(
                                         Arguments = arguments,
                                         CreateNoWindow = true,
                                         FileName = fileName,
                                         RedirectStandardError = true,
                                         RedirectStandardOutput = true,
                                         UseShellExecute = false,
                                         RedirectStandardInput = true
                                         )
         )
        proc.Start() |> ignore

        let option = if proc.WaitForExit((int) timeout.TotalMilliseconds) then proc.StandardOutput.ReadToEnd() |> Option.Some else option.None

        match option with
        | Some x -> { ExitCode = proc.ExitCode; Output = x }
        | None -> { ExitCode = proc.ExitCode; Output = String.Empty }


    let (|Int|_|) (str : string) =
       match System.Int32.TryParse(str) with
       | (true, int) -> Some(int)
       | _ -> None

    let rec getChildIdsUnix parentId (children : HashSet<int>) timeout =
        let result = runProcessAndWaitForExit "pgrep" (sprintf "-P %i" parentId) timeout
        if result.ExitCode = 0 && String.IsNullOrWhiteSpace(result.Output) = false then
            use reader = new StringReader(result.Output)
            while true do
                match reader.ReadLine() with
                | Int id ->
                    id |> children.Add |> ignore
                    (id, children, timeout) |||> getChildIdsUnix
                | _ -> ()

    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        [ runProcessAndWaitForExit "taskkill" (sprintf "/T /F /PID %i" proc.Id) timeout ]
    else
        let children = new HashSet<int>()
        (proc.Id, children, timeout) |||> getChildIdsUnix
        children |> Seq.map (fun x -> runProcessAndWaitForExit "kill" (sprintf "-TERM %i" x) timeout) |> List.ofSeq


let internal Create startInfo processOutput output  =
    let proc = new Process(StartInfo = new ProcessStartInfo(
                                     Arguments = startInfo.Arguments,
                                     CreateNoWindow = true,
                                     FileName = startInfo.FileName,
                                     RedirectStandardError = true,
                                     RedirectStandardOutput = true,
                                     UseShellExecute = false,
                                     WorkingDirectory = startInfo.WorkingDirectory,
                                     RedirectStandardInput = true
                                     )
     )

    proc.Exited |> Event.add (fun x -> (sprintf "Process exited with code '%i'." proc.ExitCode) |> output)
    proc.OutputDataReceived
    |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Output })
    |> Event.merge (
           proc.ErrorDataReceived
           |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Error })
       )
    |> Event.filter (fun x -> not (x.Data |> String.IsNullOrWhiteSpace))
    |> Event.add (processOutput)
    { Process = proc; Alias = startInfo.Alias }

let internal Run(p : FProcess) (output: string -> unit) = async {
    return using p.Process (fun proc ->
        proc.Start() |> ignore
        sprintf "Starting process %s with id %i" proc.ProcessName proc.Id |> output
        proc.BeginErrorReadLine
        >> proc.BeginOutputReadLine
        >> proc.WaitForExit |> (fun x -> x();)
    )
 }
