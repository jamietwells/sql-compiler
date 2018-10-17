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
    ( (sepBy1 (pint32  |>> ToString) (pstring "," >>. regex "\\s*")) <|> (sepBy1 (many1CharsTill anyChar (regex "\\s+")) (pstring "," >>. regex "\\s*")) )

let parseSql = 
    pstring "SELECT" >>. regex "\\s+" >>. columnParser

let compile (sql: string) = 
    match run parseSql sql with 
        | Success(result, _, _) -> Result ({ Statements =  [|{ Clause = { Columns = (result |> Array.ofList ) }}|] })
        | Failure(errorMsg, _, _) -> Fail ( errorMsg )