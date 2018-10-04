module Tests

open Xunit

let compile =
    Library.Compiler.compile "SELECT 1"

[<Fact>]
let ``SELECT 1 returns a single statement`` () =
    Assert.Single(compile.Statements)

[<Fact>]
let ``SELECT 1 returns a 'Select Statement'`` () =
    Assert.Equal("SELECT", compile.Statements.[0].Name )
