module Tests

open Xunit

let compile sql =
    SqlCompiler.compile sql

let getSuccessfulResult (result: SqlCompiler.CompilationResult) =
    match result with
    | (SqlCompiler.Result r)  -> r
    | (SqlCompiler.Fail f) -> failwith f

let firstStatement (result: SqlCompiler.SuccessfulCompilation) =
    result.Statements
    |> Array.item 0

let compileSuccessfully sql =
    sql
    |> compile
    |> getSuccessfulResult 

let clauseColumns (result: SqlCompiler.SelectStatement) = 
    result.Clause.Columns

let CheckEquality (a: 'T) (b: 'T) =
    Assert.Equal<'T>(a,b)

let CheckType<'StatementType> result =
    result
    |> Assert.IsType<'StatementType>
    |> ignore

let RandomNumbers =
    let r = System.Random()
    Seq.initInfinite (fun i -> r.Next(1, 1000))

[<Fact>]
let ``SELECT 1 returns a single statement`` () =
    let r =  RandomNumbers |> Seq.take 1 |> Array.ofSeq |> Array.item 0
    "SELECT " + r.ToString()
    |> compileSuccessfully 
    |> (fun a -> a. Statements)
    |> Array.length
    |> CheckEquality 1

[<Fact>]
let ``SELECT 1 returns a Select Statement`` () =
    compileSuccessfully "SELECT 1"
    |> firstStatement
    |> Assert.IsType<SqlCompiler.SelectStatement>

[<Fact>]
let ``SELECT 1 has a single column in ColumnList`` () =
    compileSuccessfully "SELECT 1"
    |> firstStatement
    |> clauseColumns
    |> Array.length
    |> CheckEquality 1

[<Fact>]
let ``SELECT 1 has a single column in the ColumnList with value 1`` () =
    let r =  RandomNumbers |> Seq.take 1 |> Array.ofSeq |> Array.map (fun n -> n.ToString()) |> Array.item 0
    "SELECT " + r
    |> compileSuccessfully
    |> firstStatement
    |> clauseColumns
    |> CheckEquality [| r |]

[<Fact>]
let ``SELECT 1, 2 has a two columns in ColumnList`` () =
    compileSuccessfully "SELECT 1, 2"
    |> firstStatement
    |> clauseColumns
    |> Array.length
    |> CheckEquality 2