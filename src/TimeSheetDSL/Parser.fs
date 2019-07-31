module Parser
open FParsec
open AST

type Parser<'t> = Parser<'t, unit>
exception ParsingException of string

let str_ws string = pstring string .>> spaces
let str_ws1 string = pstring string .>> spaces1
let pdecimal : Parser<_> = pfloat |>> decimal
let spacesOnly : Parser<_> = many (attempt (satisfy <| fun x -> List.contains x [' '; '\t']))

let pHoursWorked : Parser<_> = str_ws1 "hours worked" |>> fun _ -> LiteralExpression HoursWorked

let isSymbolicOperatorChar = isAnyOf ""
let remainingOpChars_ws = manySatisfy isSymbolicOperatorChar .>> spaces

let opp = new OperatorPrecedenceParser<Expression, string, unit>()
let termParser = 
    let pTrue =  str_ws1 "true" |>> fun _ -> true
    let pFalse = str_ws1 "false" |>> fun _ -> false
    let pBool = pTrue <|> pFalse |>> (BoolLiteral >> LiteralExpression)
    let pNumber = pdecimal |>> (NumberLiteral >> LiteralExpression)
    let pHoursWorked = str_ws1 "hours worked" |>> fun _ -> LiteralExpression HoursWorked
    let pApprovedOvertime =  str_ws1 <| ApprovedOverTime.ToString() |>> fun _ -> LiteralExpression ApprovedOverTime
    let pUnApprovedOvertime =  str_ws1 <| UnapprovedOverTime.ToString() |>> fun _ -> LiteralExpression UnapprovedOverTime
    let pContractor = str_ws1 <| Contractor.ToString() |>> fun _ -> LiteralExpression <| Contractor
    let pStaff = str_ws1 <| Staff.ToString() |>> fun _ -> LiteralExpression <| Staff
    let pGetEmployeeType = str_ws1 <| EmployeeType.ToString() |>> fun _ -> LiteralExpression <| EmployeeType
    let pLiteral = choice [pNumber; pBool; pHoursWorked; pApprovedOvertime; pUnApprovedOvertime; pContractor; pStaff; pGetEmployeeType]

    many (attempt (satisfy <| fun x -> List.contains x [' '; '\t'])) >>. pLiteral
    
opp.TermParser <- termParser

let addBinaryOperators prefix precedence associativity =
    let op = InfixOperator(prefix.ToString(), remainingOpChars_ws,
                           precedence, associativity, (),
                           fun _remOpChars expr1 expr2 ->
                               BinaryExpression(expr1, prefix, expr2))
    opp.AddOperator op

let addUnaryOperators prefix precedence associativity =
    let op = PrefixOperator(prefix.ToString(), remainingOpChars_ws,
                           precedence, associativity, (),
                           fun _remOpChars expr1 ->
                               UnaryExpression(prefix, expr1))
    opp.AddOperator(op)

addBinaryOperators Multiply 40 Associativity.Right
addBinaryOperators Add 30 Associativity.Right
addBinaryOperators Subtract 30 Associativity.Right
addBinaryOperators Equal 10 Associativity.Right
addBinaryOperators Less 20 Associativity.Right
addBinaryOperators LessEqual 20 Associativity.Right
addBinaryOperators Greater 20 Associativity.Right
addBinaryOperators GreaterEqual 20 Associativity.Right

addUnaryOperators SetStandardTime 5 true
addUnaryOperators SetOverTime 5 true
addUnaryOperators SetDoubleTime 5 true

let parseProgram program =
    let parsePredicate =
        sepBy opp.ExpressionParser (attempt(newline >>. spacesOnly >>. notFollowedBy (pstring "then"))) .>> spaces .>> str_ws "then"
    let parseAction =
        sepBy opp.ExpressionParser (attempt(newline >>. spacesOnly >>. notFollowedBy (pstring "end"))) .>> spaces
    let parser : Parser<RuleDeclaration> = pipe2 parsePredicate parseAction (fun a b -> (a,b))
    let parseRules = spaces >>. many (str_ws "when" >>. parser .>> str_ws "end")
    let result = run (parseRules) program

    match result with
    | Success(x, _, _) -> x : Program
    | Failure(x, _, _) -> ParsingException x |> raise
