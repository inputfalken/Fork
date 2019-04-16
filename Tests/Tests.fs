module Tests

open System
open ProcessHandler
open Xunit

[<Fact>]
let ``My test``() =
    // Assert
    let startinfo = [
        {
           Processes = []
           Alias = ""
        }
    ]
    
    let result = Command.Start.search startinfo ""
    Assert.True(true)
