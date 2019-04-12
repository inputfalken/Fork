module State
open Fork
open System
open ProcessHandler

type internal ExitResolver = {
   Handler : Handler<Choice<EventArgs, ConsoleCancelEventArgs>>
   Event : IEvent<Choice<EventArgs, ConsoleCancelEventArgs>>
 }

type internal Context = {
   InputFunction : unit -> Result<InputAnalyzer.Command, string>
   OutputFunction : string -> unit
   ActiveProcesses : FProcess list
   Processes : StartInfo list
   ProcessFactory : ProcessTask -> FProcess
   ExitResolver : ExitResolver option
 }
