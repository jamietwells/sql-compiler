open System

//====================================================

let toString o =
    o.ToString()

let charListToString chars =
    match chars with
    | [] -> ""
    | _ -> chars
        |> List.map toString
        |> List.reduce (+)

//====================================================

type ParserResult<'a> = 
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParserResult<'a * string>)

let Run parser input =
    let (Parser innerFn) = parser
    innerFn input

let EndParser parserResult =
    match parserResult with
    | Success (s, _) -> s
    | Failure (message) -> invalidOp message
    
//====================================================

let andThen parser1 parser2 =
    let innerFn input = 
        let result1 = Run parser1 input
        match result1 with
        | Failure err -> Failure err
        | Success (value1, remaining1) -> 
            let result2 = Run parser2 remaining1
            match result2 with
            | Failure err -> Failure err
            | Success (value2, remaining2) -> Success((value1, value2), remaining2)
    Parser innerFn 

let andThenLeft parser1 parser2 =
    let innerFn input = 
        let result1 = Run parser1 input
        match result1 with
        | Failure err -> Failure err
        | Success (value1, remaining1) -> 
            let result2 = Run parser2 remaining1
            match result2 with
            | Failure err -> Failure err
            | Success (_, remaining2) -> Success(value1, remaining2)
    Parser innerFn 

let andThenRight parser1 parser2 =
    let innerFn input = 
        let result1 = Run parser1 input
        match result1 with
        | Failure err -> Failure err
        | Success (_, remaining1) -> 
            let result2 = Run parser2 remaining1
            match result2 with
            | Failure err -> Failure err
            | Success (value2, remaining2) -> Success(value2, remaining2)
    Parser innerFn 
 
let orElse parser1 parser2 =
    let innerFn input = 
        let result1 = Run parser1 input
        match result1 with
        | Success s -> Success s
        | Failure err1 ->
            let result2 = Run parser2 input
            match result2 with
            | Failure err2 -> sprintf "%s or %s" err1 err2 |> Failure
            | Success s -> Success s
    Parser innerFn 

let map f parser =   
    let innerFn input =
        let result = Run parser input
        match result with 
        | Success (value, remaining) -> 
            let newValue = f value
            Success(newValue, remaining)
        | Failure f -> Failure f
    Parser innerFn

let (.>>.) = andThen
let (>>.) = andThenRight
let (.>>) = andThenLeft
let (<|>) = orElse
let (|>>) x f = map f x

//====================================================

let pcharGeneric predicate messageFn =
    let innerFn inputString =
        if String.IsNullOrEmpty(inputString) then
            Failure "Unexpected end of input"
        else if predicate inputString.[0]
        then Success (inputString.[0], inputString.[1..])
        else (messageFn inputString.[0]) |> Failure
    Parser innerFn

let pchar character =
    pcharGeneric (fun c -> c = character) (sprintf "Expected '%c' got '%c'" character)

let pcharIgnoreCase character =
    pcharGeneric (fun c -> (c |> Char.ToLowerInvariant) = (character |> Char.ToLowerInvariant)) (fun c -> (sprintf "Expected '%c' got '%c'" character c))

let pCharAny =
    pcharGeneric (fun _ -> true) (fun _ -> "")

let pCharExcept chars =
    pcharGeneric (fun c -> chars |> List.contains c |> not) (sprintf "Invalid character '%c'")

let pManyGeneric charMap concatFn parser =
    let rec recursiveInner (prevVal, prevRemaining) =
        let result1 = Run parser prevRemaining
        match result1 with
        | Failure _ -> Success (prevVal, prevRemaining)
        | Success (value, remaining) -> recursiveInner ((concatFn prevVal (value |> charMap)), remaining) 
    let innerFn inputString =
        let result1 = Run parser inputString
        match result1 with
        | Failure err -> Failure err
        | Success (value, remaining) -> recursiveInner (value |> charMap, remaining)
    Parser innerFn


let pManyOrNone parser =
    let rec recursiveInner (prevVal, prevRemaining) =
        let result1 = Run parser prevRemaining
        match result1 with
        | Failure _ -> Success (prevVal, prevRemaining)
        | Success (value, remaining) -> recursiveInner (prevVal@[value], remaining) 
    Parser (fun s -> recursiveInner ([], s))

let pMany parser =
    let rec recursiveInner (prevVal, prevRemaining) =
        let result1 = Run parser prevRemaining
        match result1 with
        | Failure _ -> Success (prevVal, prevRemaining)
        | Success (value, remaining) -> recursiveInner (prevVal@[value], remaining) 
    let innerFn inputString =
        let result1 = Run parser inputString
        match result1 with
        | Failure err -> Failure err
        | Success (value, remaining) -> recursiveInner ([value], remaining)
    Parser innerFn

let pCharAnyOf characters =
    characters 
    |> List.map pchar
    |> List.reduce (<|>)

let pCharManyOf characters =
    characters
    |> pCharAnyOf
    |> pMany
    |> map charListToString

let matchCharsGeneric characters parser =
    characters
    |> Array.map (parser >> (fun p -> p |>> string))
    |> Array.reduce (fun p1 p2 -> (p1 .>>. p2) |>> ((fun (c1, c2) -> c1 + c2 )))

let matchChars characters =
    matchCharsGeneric characters pchar

let matchCharsInv characters =
    matchCharsGeneric characters pcharIgnoreCase

let matchStrInv str =
    (string str).ToCharArray() 
    |> matchCharsInv

let matchStr str = 
    (string str).ToCharArray() 
    |> matchChars
 
let matchMapStr (str: string, mapping) = 
    matchStr str 
    |>> mapping
 
let matchAnyStrGeneric mapping strings =
    strings 
    |> List.map mapping
    |> List.reduce (<|>)

let matchAnyStr strings =
    strings 
    |> matchAnyStrGeneric matchStr

let matchMapAnyStr strs =
    strs
    |> matchAnyStrGeneric matchMapStr

//====================================================

type StringValue = { Value: string }
type IntValue = { Value: int }
type Whitespace = StringValue
type Identifier = StringValue
type WildcardIdentifier = Identifier
type QuotedIdentifier = Identifier
type UnquotedIdentifier = Identifier
type TableName = Identifier
type KeywordValue = StringValue
type FromKeyword = KeywordValue
type IntoKeyword = KeywordValue
type SetKeyword = KeywordValue
type SelectKeyword = KeywordValue
type InsertKeyword = KeywordValue
type UpdateKeyword = KeywordValue
type DeleteKeyword = KeywordValue

type AssignedValue = 
    AssignedIntegerValue of IntValue
    | AssignedStringValue of StringValue
    | AssignedIdentifier of Identifier

type SelectStatement = { Keyword: SelectKeyword; Identifiers: Identifier list; FromKeyword: FromKeyword; TableName: Identifier }
type InsertStatement = { Keyword: InsertKeyword; Identifiers: Identifier list; IntoKeyword: IntoKeyword; TableName: Identifier }
type UpdateStatement = { Keyword: UpdateKeyword; Identifiers: (Identifier * AssignedValue) list; SetKeyword: SetKeyword; TableName: Identifier }
type DeleteStatement = { Keyword: DeleteKeyword; FromKeyword: KeywordValue; TableName: Identifier }

type SqlStatement = 
    SqlSelectStatement of SelectStatement 
    | SqlInsertStatement of InsertStatement 
    | SqlUpdateStatement of UpdateStatement 
    | SqlDeleteStatement of DeleteStatement 

//====================================================

let AssignedIdentifierConstructor identifier =
    AssignedIdentifier identifier

let AssignedIntegerConstructor str =
    AssignedIntegerValue { Value = Int32.Parse str }

let AssignedStringConstructor str =
    AssignedStringValue { Value = str }

let StringValueConstructor str =
    { Value = str }: StringValue

let WhitespaceConstructor str =
    { Value = str }: Whitespace

let IdentifierConstructor str =
    { Value = str }: Identifier

let WildcardIdentifierConstructor str =
    { Value = str }: WildcardIdentifier

let QuotedIdentifierConstructor str =
    { Value = str }: QuotedIdentifier

let UnquotedIdentifierConstructor str =
    { Value = str }: UnquotedIdentifier

let KeywordValueConstructor str =
    { Value = str }: KeywordValue

let FromKeywordConstructor str =
    { Value = str } : FromKeyword
    
let IntoKeywordConstructor str =
    { Value = str } : IntoKeyword
    
let SetKeywordConstructor str =
    { Value = str } : SetKeyword
    
let SelectKeywordConstructor str =
    { Value = str } : SelectKeyword
    
let InsertKeywordConstructor str =
    { Value = str } : InsertKeyword
    
let UpdateKeywordConstructor str =
    { Value = str } : UpdateKeyword
    
let DeleteKeywordConstructor str =
    { Value = str } : DeleteKeyword

//====================================================

let prepend (a, b) =
    [a]@b

let append (a, b) =
    a@[b]

let singleWhitespaceParser = pCharAnyOf [' '; '\t'; '\n']

let requiredWhitespaceParser = 
    pMany singleWhitespaceParser
    |>> charListToString
    |>> WhitespaceConstructor

let optionalWhitespaceParser = 
    pManyOrNone singleWhitespaceParser
    |>> charListToString
    |>> WhitespaceConstructor

let fromKeywordParser = 
    matchStrInv "From" 
    |>> FromKeywordConstructor

let intoKeywordParser = 
    matchStrInv "Into" 
    |>> IntoKeywordConstructor

let setKeywordParser = 
    matchStrInv "Set" 
    |>> SetKeywordConstructor

let selectKeywordParser = 
    matchStrInv "Select" 
    |>> SelectKeywordConstructor

let insertKeywordParser = 
    matchStrInv "Insert" 
    |>> InsertKeywordConstructor

let updateKeywordParser = 
    matchStrInv "Update" 
    |>> UpdateKeywordConstructor

let deleteKeywordParser = 
    matchStrInv "Delete" 
    |>> DeleteKeywordConstructor

let namedIdentifierParser = 
    pCharAnyOf (['a'..'z'] @ ['A'..'Z'] @ ['_']) 
    .>>. pManyOrNone (pCharAnyOf (['a'..'z'] @ ['A'..'Z'] @ ['0'..'9'] @ ['_'])) 
    |>>  prepend
    |>>  charListToString
    |>>  UnquotedIdentifierConstructor

let quotedIdentifierNamedParser = 
    pchar '[' 
    >>. pMany (pCharExcept [']']) 
    .>> pchar ']'
    |>> charListToString
    |>> QuotedIdentifierConstructor

let wildcardNamedParser = 
    pchar '*' 
    |>> toString 
    |>> WildcardIdentifierConstructor

let someNamedIdentifierParser = namedIdentifierParser <|> quotedIdentifierNamedParser 

let someIdentifierParser = someNamedIdentifierParser <|> wildcardNamedParser


let stringParser =
    pchar '\''
    >>. pManyOrNone (pCharExcept ['\''])
    .>> pchar '\''
    |>> charListToString

let intParser =
    pMany (pCharAnyOf ['0' .. '9'])
    |>> charListToString

let someAssignmentResultParser = 
    (stringParser |>> AssignedStringConstructor)
    <|> (intParser |>> AssignedIntegerConstructor)
    <|> (someNamedIdentifierParser |>> AssignedIdentifierConstructor)

let someAssignmentParser =
    someNamedIdentifierParser
    .>> optionalWhitespaceParser
    .>> pchar '='
    .>> optionalWhitespaceParser
    .>>. someAssignmentResultParser

let commaArrayGeneric parser = 
    pManyOrNone (parser .>> optionalWhitespaceParser .>> pchar ',' .>> optionalWhitespaceParser) 
    .>>. parser
    |>> append

let identifierArray = 
    commaArrayGeneric someIdentifierParser

let assignmentArray = 
    commaArrayGeneric someAssignmentParser

let selectStatementParser =
    selectKeywordParser 
    .>> requiredWhitespaceParser 
    .>>. identifierArray 
    .>> requiredWhitespaceParser 
    .>>. fromKeywordParser
    .>> requiredWhitespaceParser 
    .>>. someNamedIdentifierParser
    |>> fun (((skw, uqil), fkw), uqi) -> SqlSelectStatement { Keyword = skw; Identifiers = uqil; FromKeyword = fkw; TableName = uqi }

let updateStatementParser =
    updateKeywordParser 
    .>> requiredWhitespaceParser 
    .>>. someNamedIdentifierParser 
    .>> requiredWhitespaceParser 
    .>>. setKeywordParser
    .>> requiredWhitespaceParser 
    .>>. assignmentArray 
    |>> fun (((ukw, uqi), skw), uqil) -> SqlUpdateStatement { Keyword = ukw; Identifiers = uqil; TableName = uqi; SetKeyword = skw }

let deleteStatementParser =
    deleteKeywordParser 
    .>> requiredWhitespaceParser 
    .>>. fromKeywordParser 
    .>> requiredWhitespaceParser 
    .>>. someNamedIdentifierParser
    |>> fun ((dkw, fkw), ni) -> SqlDeleteStatement { Keyword = dkw; TableName = ni; FromKeyword = fkw }

let master =
    (selectStatementParser <|> updateStatementParser <|> deleteStatementParser)

//====================================================

let sql = [
    "SELECT * FROM abc"; 
    "SELECT a1, _a1, [ hello] , abc, a, * FROM abc"; 
    "UPDATE abc SET a = 'hello', b = [column B], c = columnC";
    "DELETE FROM abc";
]

let printResult result =
    result
    |> printf "%A\n"

let runTest text =
    let result = Run master text |> EndParser
    printf "=========================================\n\n"
    printResult text
    printResult result
    printf "\n=========================================\n\n"

sql 
    |> List.map runTest
    |> ignore