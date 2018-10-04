module Tests

open Xunit
open Library

let compile =
    SqlCompiler.compile "SELECT 1"

let firstStatement (result: CompilationResult) =
    result.Statements.[0]

let getFirstStatement =
    compile |> firstStatement

[<Fact>]
let ``SELECT 1 returns a single statement`` () =
    Assert.Single(compile.Statements)

[<Fact>]
let ``SELECT 1 returns a 'Select Statement'`` () =
    Assert.Equal("SELECT", getFirstStatement.Name )

[<Fact>]
let ``SELECT 1 has a single column in 'ColumnList'`` () =
    Assert.Single(getFirstStatement.ColumnList)

[<Fact>]
let ``SELECT 1 has a single column in the 'ColumnList' with value 1`` () =
    Assert.Equal("1", getFirstStatement.ColumnList.[0].Value)