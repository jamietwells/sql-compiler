namespace Library

type CompilationResult = { Statements: string[] }

module Compiler =
    let compile sql =
         { Statements = Array.empty } 
