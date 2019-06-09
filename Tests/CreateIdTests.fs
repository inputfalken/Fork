module Tests.CreateIdTests
open ProcessHandler
open Tests
open Xunit

[<Fact>]
let ``Without number``() =
    Assert.Equal(0, Command.Start.CreateId "foo")

[<Fact>]
let ``With 0``() =
    Assert.Equal(1, Command.Start.CreateId "foo0")

[<Fact>]
let ``With 1``() =
    Assert.Equal(2, Command.Start.CreateId "foo1")

[<Fact>]
let ``With 10``() =
    Assert.Equal(11, Command.Start.CreateId "foo10")

[<Fact>]
let ``With 11``() =
    Assert.Equal(12, Command.Start.CreateId "foo10")

[<Fact>]
let ``With 100``() =
    Assert.Equal(101, Command.Start.CreateId "foo10")

[<Fact>]
let ``With 1000``() =
    Assert.Equal(1001, Command.Start.CreateId "foo10")
