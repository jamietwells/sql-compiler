module SqlCompiler

type ConstantValue = { Value: string }
type Clause = { Columns: string[] }
type SelectStatement = { Clause: Clause }
type Statement = SelectStatement
type CompilationResult = { Statements: Statement[] }
let compile sql =
    { 
        Statements = Array.create 1 {
            Clause = {
                Columns = Array.create 1 "1"
            }
        }
    }