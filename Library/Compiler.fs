namespace Library

type ConstantValue = { Value: string }
type Statement = { Name: string; ColumnList: ConstantValue[] }
type CompilationResult = { Statements: Statement[] }

module SqlCompiler =
    let compile sql =
         { Statements = Array.create 1 { Name = "SELECT"; ColumnList = Array.create 1 { Value = "1" } } }