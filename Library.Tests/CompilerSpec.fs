module Tests

open Xunit
open System.Collections.Generic
let random = System.Random()

let compile sql =
    SqlCompiler.compile sql

let getSuccessfulResult (result: SqlCompiler.CompilationResult) =
    match result with
    | (SqlCompiler.Result r) -> r
    | (SqlCompiler.Fail f) -> failwith f

let getFailureResult (result: SqlCompiler.CompilationResult) =
    match result with
    | (SqlCompiler.Result r) -> failwith "Expected failure"
    | (SqlCompiler.Fail f) -> f

let selectFirstStatement (result: SqlCompiler.SuccessfulCompilation) =
    result.Statements
    |> Array.item 0

let compileSuccessfully sql =
    sql
    |> compile
    |> getSuccessfulResult 

let compileWithFailure sql =
    sql
    |> compile
    |> getFailureResult 

let selectClauseColumns (result: SqlCompiler.SelectStatement) = 
    result.Clause.Columns

let CheckEquality (a: 'T) (b: 'T) =
    Assert.Equal<'T>(a,b)

let CheckType<'StatementType> result =
    result
    |> Assert.IsType<'StatementType>
    |> ignore

let RandomNumbers =
    Seq.initInfinite (fun i -> random.Next(1, 1000))

let RandomIntsAsStrings count =
    RandomNumbers 
    |> Seq.take count 
    |> Array.ofSeq 
    |> Array.map (fun n -> n.ToString())

let toCommaSeparatedString items =
    items
    |> Array.ofSeq
    |> Array.map (fun n -> n.ToString())
    |> String.concat ", "

let pickAtRandom (items: 'a array) =
    items
    |> Array.item (random.Next(0, items.Length))

let randomLetter() =
    (['a'..'z'] @ ['A'..'Z'])
    |> Array.ofSeq
    |> pickAtRandom
    |> (fun c -> c.ToString())

let randomWord () =
    Seq.init (random.Next(1, 15)) (fun i -> randomLetter())
    |> String.concat ""

let randomWhitespace () =
    Seq.init (random.Next(1, 15)) (fun i -> pickAtRandom [|' '; '\t'; '\n'|])
    |> Seq.map (fun c -> c.ToString())
    |> String.concat ""

let joinTwoStrings (s1: string) s2 =
    s1 + s2

[<Fact>]
let ``SELECT 1 returns a single statement`` () =
    let r =
        RandomIntsAsStrings 1
        |> Array.item 0
    r
    |> joinTwoStrings "SELECT "
    |> compileSuccessfully 
    |> (fun a -> a. Statements)
    |> Array.length
    |> CheckEquality 1

[<Fact>]
let ``SELECT 1 returns a Select Statement`` () =
    compileSuccessfully "SELECT 1"
    |> selectFirstStatement
    |> Assert.IsType<SqlCompiler.SelectStatement>

[<Fact>]
let ``SELECT 1 has a single column in ColumnList`` () =
    compileSuccessfully "SELECT 1"
    |> selectFirstStatement
    |> selectClauseColumns
    |> Array.length
    |> CheckEquality 1
   
[<Fact>]
let ``Clause columns are correct for only one column in SELECT`` () =
    let r = 
        RandomIntsAsStrings 1
        |> Array.item 0
    r
    |> joinTwoStrings "SELECT "
    |> compileSuccessfully
    |> selectFirstStatement
    |> selectClauseColumns
    |> CheckEquality [| r |]

[<Fact>]
let ``Correct number of clause columns returned`` () =
    100
    |> RandomIntsAsStrings
    |> String.concat ", "
    |> joinTwoStrings "SELECT " 
    |> compileSuccessfully
    |> selectFirstStatement
    |> selectClauseColumns
    |> Array.length
    |> CheckEquality 100

[<Fact>]
let ``Correct values of clause columns returned`` () =
    let r = 
        RandomIntsAsStrings 100
    r
    |> String.concat ", "
    |> joinTwoStrings "SELECT "
    |> compileSuccessfully 
    |> selectFirstStatement
    |> selectClauseColumns
    |> CheckEquality r

[<Fact>]
let ``Selecting nothing is a failure to compile`` () =
    "SELECT"
    |> compileWithFailure

[<Fact>]
let ``Selecting whitespace is a failure to compile`` () =
    "SELECT "
    |> compileWithFailure

[<Fact>]
let ``Can select column names where from a table`` () =
    let colName = randomWord()
    [|"SELECT" ; randomWhitespace() ; colName ; randomWhitespace() ; "FROM"; randomWhitespace() ; randomWord()|]
    |> String.concat ""
    |> compileSuccessfully
    |> selectFirstStatement
    |> selectClauseColumns
    |> CheckEquality [|colName|]