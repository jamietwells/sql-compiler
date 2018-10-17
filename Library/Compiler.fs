module SqlCompiler

open FParsec
type ConstantValue = { Value: string }
type Clause = { Columns: string[] }
type SelectStatement = { Clause: Clause }
type Statement = SelectStatement
type SuccessfulCompilation = { Statements: Statement[] } 
type CompilationResult = 
    | Result of SuccessfulCompilation
    | Fail of string

let ToString a =
    a.ToString()

let columnParser =
    ( (sepBy1 (pint32  |>> ToString) (pstring ", ")) <|> (sepBy1 (many1CharsTill anyChar (pstring " ")) (pstring ", ")) )

let parseSql = 
    pstring "SELECT " >>. columnParser

let compile (sql: string) = 
    match run parseSql sql with 
        | Success(result, _, _) -> Result ({ Statements =  [|{ Clause = { Columns = (result |> Array.ofList ) }}|] })
        | Failure(errorMsg, _, _) -> Fail ( errorMsg )