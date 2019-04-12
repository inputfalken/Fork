module public ProcessHandler
open System.Diagnostics
open System
open System.IO
open System.Runtime.InteropServices

type public ProcessResult = { ExitCode : int; Output : string }
type public ProcessExit = { Code : int; Time : DateTime }
type public Stdout = | Error | Output
type public OutputType = { Data : string; Type : Stdout }
type public ProcessTask = { WorkingDirectory : string; FileName : string; Arguments : string; Alias : string; UseSeperateWindow : bool; }
type public FProcess = { Process : Process; Alias : string; Arguments : ProcessTask }
type public Task = { Tasks : ProcessTask list; Alias : string }
type public StartInfo = { Processes : FProcess list; Alias : String }

// Taken from https://github.com/aspnet/Extensions/blob/ffb7c20fb22a31ac31d3a836a8455655867e8e16/shared/Microsoft.Extensions.Process.Sources/ProcessHelper.cs
let public RecursiveKill (proc : Process) (timeout : TimeSpan) =
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

    let rec grepChildProcesses set parentId timeout =
        let rec processLine line pId (set : Set<int>) =
            use reader = new StringReader(line)
            match reader.ReadLine() with
            | Int id ->
                id |> set.Add |> ignore
                (line, id, set) |||> processLine
            | _ -> (set, pId)


        let result = runProcessAndWaitForExit "pgrep" (sprintf "-P %i" parentId) timeout
        if result.ExitCode = 0 then
            processLine result.Output parentId set |> (fun (x, y) -> grepChildProcesses x y timeout)
        else set


    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
        [ runProcessAndWaitForExit "taskkill" (sprintf "/T /F /PID %i" proc.Id) timeout ]
    else
        (Set.empty, proc.Id, timeout)
        |||> grepChildProcesses
        |> Set.map (fun x -> runProcessAndWaitForExit "kill" (sprintf "-TERM %i" x) timeout)
        |> Set.toList


let public Create task processOutput output =
    task.WorkingDirectory
    |> Option.Some
    |> Option.filter (fun x -> not (Directory.Exists x))
    |> Option.iter (fun x -> raise (ArgumentException (sprintf "The value '%s' is not a valid directory path." x)))

    let proc = match task.UseSeperateWindow with
               | true -> new Process(StartInfo = new ProcessStartInfo(
                                                Arguments = task.Arguments,
                                                CreateNoWindow = true,
                                                FileName = task.FileName,
                                                RedirectStandardError = false,
                                                RedirectStandardInput = false,
                                                RedirectStandardOutput = false,
                                                UseShellExecute = true,
                                                WorkingDirectory = task.WorkingDirectory
                                              ))
               | false -> new Process(StartInfo = new ProcessStartInfo(
                                                Arguments = task.Arguments,
                                                CreateNoWindow = true,
                                                FileName = task.FileName,
                                                RedirectStandardError = true,
                                                RedirectStandardInput = true,
                                                RedirectStandardOutput = true,
                                                UseShellExecute = false,
                                                WorkingDirectory = task.WorkingDirectory
                                              ))

    if task.UseSeperateWindow = false then
        proc.Exited |> Event.add (fun x -> (sprintf "Process exited with code '%i'." proc.ExitCode) |> output)
        proc.OutputDataReceived
        |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Output })
        |> Event.merge (
               proc.ErrorDataReceived
               |> Event.map (fun x -> { Data = x.Data; Type = Stdout.Error })
           )
        |> Event.filter (fun x -> not (x.Data |> String.IsNullOrWhiteSpace))
        |> Event.add (processOutput)
    { Process = proc; Alias = task.Alias; Arguments = task }

let public Run p output = async {
    return using p.Process (fun proc ->
        proc.Start() |> ignore
        sprintf "Starting process %s with id %i" proc.ProcessName proc.Id |> output
        if p.Arguments.UseSeperateWindow = false then
            proc.BeginErrorReadLine >> proc.BeginOutputReadLine |> (fun x -> x();)
        proc.WaitForExit()
    )
 }
