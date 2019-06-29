module State
open Fork
open System
open ProcessHandler

type internal ExitResolver<'T> = {
   Handler : Handler<Choice<EventArgs, 'T>>
   Event : IEvent<Choice<EventArgs, 'T>>
 }

type internal Context<'T> = {
   InputFunction : unit -> Result<InputAnalyzer.Command, string>
   OutputFunction : string -> unit
   ActiveProcesses : FProcess list
   Processes : StartInfo list
   ProcessFactory : ProcessTask -> FProcess
   ExitResolver : ExitResolver<'T> option
   OnExit : IEvent<'T>
 }

type SearchResult = | Alias of FProcess Option * string
                    | AliasGroup of FProcess List * string


