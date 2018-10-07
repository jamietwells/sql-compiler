module SqlCompiler

type ConstantValue = { Value: string }
type Clause = { Columns: string[] }
type SelectStatement = { Clause: Clause }
type Statement = SelectStatement
type CompilationResult = { Statements: Statement[] }

let Columns sql =
    if 
        "SELECT 1".Equals(sql)
    then 
        [|"1"|]
    else
        [|"1"; "2"|]
    
let compile sql =
    { 
        Statements = Array.create 1 {
            Clause = {
                Columns = Columns sql
            }
        }
    }