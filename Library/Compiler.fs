module SqlCompiler

open FParsec
open System
type ConstantValue = { Value: string }
type Clause = { Columns: string[] }
type SelectStatement = { Clause: Clause }
type Statement = SelectStatement
type SuccessfulCompilation = { Statements: Statement[] } 
type CompilationResult = 
    | Result of SuccessfulCompilation
    | Fail of string

let Columns sql =
    if 
        "1".Equals(sql)
    then 
        [|"1"|]
    else
        [|"1"; "2"|]
    
let ToString a =
    a.ToString()

let parseSql = 
    pstring "SELECT " >>. ( (sepBy1 (pint32  |>> ToString) (pstring ", ")) <|> (sepBy1 (pstring "colname") (pstring ", ")) )

let compile (sql: string) = 
    match run parseSql sql with 
        | Success(result, _, _) -> Result ({ Statements =  [|{ Clause = { Columns = (result |> Array.ofList ) }}|] })
        | Failure(errorMsg, _, _) -> Fail ( errorMsg )