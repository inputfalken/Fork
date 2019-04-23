module Tests.StartSearchTests

open ProcessHandler
open Xunit

[<Fact>]
let ``Search with match for alias group``() =
    match Command.Start.search DummyData.startinfo DummyData.groupAlias with
    | State.SearchResult.Alias(x, y) -> Assert.True(false, "This should be a alias group result")
    | State.SearchResult.AliasGroup(x, y) ->
        Assert.Equal(x.Length, 2)
        Assert.Equal(x.[0].Alias, DummyData.processAlias1)
        Assert.Equal(x.[1].Alias, DummyData.processAlias2)

[<Fact>]
let ``Search with match for alias``() =
    match Command.Start.search DummyData.startinfo DummyData.processAlias1 with
    | State.SearchResult.Alias(x, y) ->
        Assert.True(x.IsSome)
        Assert.Equal(DummyData.processAlias1, x.Value.Alias)
    | State.SearchResult.AliasGroup(x, y) -> Assert.True(false, "This should be an alias result.")

[<Fact>]
let ``Search with no result``() =
    match Command.Start.search DummyData.startinfo "x" with
    | State.SearchResult.Alias(x, y) ->
        Assert.True(x.IsNone)
    | State.SearchResult.AliasGroup(x, y) -> Assert.True(false, "This should be an alias result.")
