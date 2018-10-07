module Tests

open Xunit

let compile sql =
    SqlCompiler.compile sql

let firstStatement (result: SqlCompiler.CompilationResult) =
    result.Statements
    |> Array.item 0

let clauseColumns (result: SqlCompiler.SelectStatement) = 
    result.Clause.Columns

let CheckEquality (a: 'T) (b: 'T) =
    Assert.Equal<'T>(a,b)

let CheckType<'StatementType> result =
    result
    |> Assert.IsType<'StatementType>
    |> ignore

[<Fact>]
let ``SELECT 1 returns a single statement`` () =
    compile "SELECT 1"
    |> (fun a -> a.Statements)
    |> Array.length
    |> CheckEquality 1

[<Fact>]
let ``SELECT 1 returns a Select Statement`` () =
    compile "SELECT 1"
    |> firstStatement
    |> Assert.IsType<SqlCompiler.SelectStatement>

[<Fact>]
let ``SELECT 1 has a single column in ColumnList`` () =
    compile "SELECT 1"
    |> firstStatement
    |> clauseColumns
    |> Array.length
    |> CheckEquality 1

[<Fact>]
let ``SELECT 1 has a single column in the ColumnList with value 1`` () =
    compile "SELECT 1"
    |> firstStatement
    |> clauseColumns
    |> CheckEquality [|"1"|]

// [<Fact>]
// let ``SELECT 1, 2 has a two columns in ColumnList`` () =
//     compile "SELECT 1, 2"
//     |> firstStatement
//     |> clauseColumns
//     |> Array.length
//     |> CheckEquality 2