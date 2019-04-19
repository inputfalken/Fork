module Tests

open ProcessHandler
open Xunit

[<Fact>]
let ``Search with match for alias group``() =
    // Assert
    let startinfo = [
        {
           Processes = [
               {
                   Alias = "testing"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
               {
                   Alias = "testing2"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
           ]
           Alias = "Foo"
        }
    ]

    match Command.Start.search startinfo "Foo" with
    | State.SearchResult.Alias(x, y) -> Assert.True(false, "This should be a alias group result")
    | State.SearchResult.AliasGroup(x, y) ->
        Assert.Equal(x.Length, 2)
        Assert.Equal(x.[0].Alias, "testing")
        Assert.Equal(x.[1].Alias, "testing2")

[<Fact>]
let ``Search with match for alias``() =
    // Assert
    let startinfo = [
        {
           Processes = [
               {
                   Alias = "testing"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
               {
                   Alias = "testing2"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
           ]
           Alias = "Foo"
        }
    ]

    match Command.Start.search startinfo "testing" with
    | State.SearchResult.Alias(x, y) ->
        Assert.True(x.IsSome)
        Assert.Equal("testing", x.Value.Alias)
    | State.SearchResult.AliasGroup(x, y) -> Assert.True(false, "This should be an alias result.")
    
[<Fact>]
let ``Search with no result``() =
    // Assert
    let startinfo = [
        {
           Processes = [
               {
                   Alias = "testing"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
               {
                   Alias = "testing2"
                   Arguments = {
                        WorkingDirectory = "";
                        FileName = ""
                        Arguments = ""
                        Alias = ""
                        UseSeperateWindow = true
                   }
                   Process = null
               }
           ]
           Alias = "Foo"
        }
    ]

    match Command.Start.search startinfo "x" with
    | State.SearchResult.Alias(x, y) ->
        Assert.True(x.IsNone)
    | State.SearchResult.AliasGroup(x, y) -> Assert.True(false, "This should be an alias result.")
