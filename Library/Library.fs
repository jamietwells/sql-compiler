namespace Library

type Statement = { Name: string }
type CompilationResult = { Statements: Statement[] }

module Compiler =
    let compile sql =
         { Statements = Array.create 1 { Name = "SELECT" } } 
