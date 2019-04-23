module Tests.StopSearchTests
open ProcessHandler
open Tests
open Xunit

[<Fact>]
let ``Search with match for alias group with all active process``() =
    let activeProcesses = DummyData.startinfo
                          |> List.collect (fun x -> x.Processes)

    match Command.Stop.search DummyData.startinfo activeProcesses DummyData.groupAlias with
    | State.SearchResult.Alias(x, y) -> Assert.True(false, "This should be a alias group result")
    | State.SearchResult.AliasGroup(x, y) ->
        Assert.Equal(x.Length, 2)
        Assert.Equal(x.[0].Alias, DummyData.processAlias1)
        Assert.Equal(x.[1].Alias, DummyData.processAlias2)

[<Fact>]
let ``Search with match for alias group with one active process``() =
    let activeProcesses = DummyData.startinfo
                          |> List.collect (fun x -> x.Processes)
                          |> List.take 1
                          
    match Command.Stop.search DummyData.startinfo activeProcesses DummyData.groupAlias with
    | State.SearchResult.Alias(x, y) -> Assert.True(false, "This should be a alias group result")
    | State.SearchResult.AliasGroup(x, y) ->
        Assert.Equal(x.Length, 1)
        Assert.Equal(x.[0].Alias, DummyData.processAlias1)
