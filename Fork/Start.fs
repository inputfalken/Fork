module internal Command.Start
open ProcessHandler
open State
open System


let internal CreateId(alias : string) =
   (alias, alias) ||> (fun x y ->
        let lastchar = x.[y.Length - 1]
        if lastchar |> Char.IsNumber
            then
                 x
                 |> Seq.rev
                 |> Seq.takeWhile Char.IsNumber
                 |> Seq.rev
                 |> Seq.toArray
                 |> String
                 |> Int32.Parse
                 |> (+) 1
            else 0
    )

let internal search (processes : StartInfo list) (input : string) =
    let noAliasGroupSearch() =
        processes
        |> List.collect (fun x -> x.Processes)
        |> List.filter (fun x -> x.Alias = input)
        |> (fun x -> if x.IsEmpty then Option.None else Option.Some x.[0])

    let search = processes |> List.filter (fun x -> x.Alias = input)

    if search.IsEmpty
        then (noAliasGroupSearch(), input) |> SearchResult.Alias
        else (search |> List.collect (fun x -> x.Processes), input) |> SearchResult.AliasGroup

let internal Exec input processes (activeProcesses : FProcess list) startProcess =
    match search processes input with
                    | Alias(x, y) -> match x with
                                     | Some x -> [ x ]
                                     | None -> []
                    | AliasGroup(x, y) -> x
                    |> List.map (fun x ->
                                     let alias = if activeProcesses |> List.exists (fun y -> y.Alias = x.Alias)
                                                 then (x.Alias, CreateId x.Alias) ||> sprintf "%s-%i"
                                                 else x.Alias
                                     // Are the two alias properties really needed???
                                     {
                                         Process = x.Process
                                         Alias = alias
                                         Arguments = {
                                             Alias = alias
                                             Arguments = x.Arguments.Arguments
                                             FileName = x.Arguments.FileName
                                             WorkingDirectory = x.Arguments.WorkingDirectory
                                             UseSeperateWindow = x.Arguments.UseSeperateWindow

                                         }
                                     }
                                 )
                    |> List.map (fun x -> x.Arguments |> startProcess)
                    |> (fun x -> activeProcesses @ x)
