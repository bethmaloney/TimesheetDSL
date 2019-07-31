module Interpreter
open AST

exception RuntimeException of string;

let raiseInvalidOperator() =
    RuntimeException "Cannot apply operator to this type" |> raise
let raiseMismatchType() =
    RuntimeException "Cannot apply an operator to different types" |> raise

type HoursSpread = {mutable standardTime : decimal; mutable overTime : decimal; mutable doubleTime : decimal}
type HoursWorkedPerDay = {HoursWorked : decimal; IsOTApproved : bool}
type EmployeeType =
    | Contractor
    | Staff
type Employee = {Name : string; HoursWorkedPerDay : HoursWorkedPerDay list; EmployeeType : EmployeeType}

type ExpressionType =
    | Number of decimal
    | Bool of bool
    | String of string
    | Employee of EmployeeType
    | None

let execute employee (program : Program) =
    let output =  {standardTime = decimal 0; overTime = decimal 0; doubleTime = decimal 0}

    let hoursWorked =
        List.sumBy (fun x -> x.HoursWorked) employee.HoursWorkedPerDay 

    let approvedOverTime =
        employee.HoursWorkedPerDay
        |> List.where (fun x -> x.IsOTApproved && x.HoursWorked > 8M)
        |> List.sumBy (fun x -> x.HoursWorked - 8M)

    let unapprovedOverTime =
        employee.HoursWorkedPerDay
        |> List.where (fun x -> not x.IsOTApproved && x.HoursWorked > 8M)
        |> List.sumBy (fun x -> x.HoursWorked - 8M)

    let executeLiteral literal =
        match literal with
        | BoolLiteral(x)             -> Bool(x)
        | NumberLiteral(x)           -> Number(x)
        | StringLiteral(x)           -> String(x)
        | HoursWorked                -> Number(hoursWorked)
        | ApprovedOverTime           -> Number(approvedOverTime)
        | UnapprovedOverTime         -> Number(unapprovedOverTime)
        | Literal.Contractor         -> Employee Contractor
        | Literal.Staff              -> Employee Staff
        | Literal.EmployeeType       -> Employee employee.EmployeeType
    
    let rec executeBinaryExpression (exp1 : Expression) op exp2 =
        let expRun1 = executeExpression exp1
        let expRun2 = executeExpression exp2

        match (expRun1, expRun2) with
        | (Bool(arg1), Bool(arg2)) ->
            match op with
            | ConditionalOr ->
                Bool(arg1 || arg2)
            | Equal -> 
                Bool(arg1 = arg2)
            | NotEqual -> 
                Bool(arg1 <> arg2)
            | ConditionalAnd -> 
                Bool(arg1 && arg2)
            | Add | Assignment | Divide | Greater | GreaterEqual | Less | LessEqual | Modulus | Multiply | Subtract ->
                raiseInvalidOperator()
        | (Number(arg1), Number(arg2)) ->
            match op with
            | Equal ->
                Bool(arg1 = arg2)
            | NotEqual ->
                Bool(arg1 <> arg2)
            | LessEqual ->
                Bool(arg1 <= arg2)
            | Less ->
                Bool(arg1 < arg2)
            | GreaterEqual ->
                Bool(arg1 >= arg2)
            | Greater ->
                Bool(arg1 > arg2)
            | Subtract ->
                Number(arg1 - arg2)
            | Multiply ->
                Number(arg1 * arg2)
            | Divide ->
                Number(arg1 / arg2)
            | Modulus ->
                Number(arg1 % arg2)
            | Add ->
                Number(arg1 + arg2)
            | Assignment | ConditionalAnd | ConditionalOr ->
                raiseInvalidOperator()
        | (String(arg1), String(arg2)) ->
            match op with
            | Equal ->
                Bool(arg1 = arg2)
            | _ ->
                raiseInvalidOperator()
        | (Employee(arg1), Employee(arg2)) ->
            match op with
            | Equal ->
                Bool(arg1 = arg2)
            | _ ->
                raiseInvalidOperator()
        | _ -> raiseMismatchType()

    and executeUnaryExpression op exp1 =
        let expressionResult = executeExpression exp1
        match op with
        | LogicalNegate ->
            match expressionResult with
            | Bool(x) -> Bool(not x)
            | _ -> raiseMismatchType()
        | SetStandardTime ->
            match expressionResult with
            | Number(x) ->
                output.standardTime <- x
                None
            | _ -> raiseMismatchType()
        | SetOverTime ->
            match expressionResult with
            | Number(x) ->
                output.overTime <- x
                None
            | _ -> raiseMismatchType()
        | SetDoubleTime ->
            match expressionResult with
            | Number(x) ->
                output.doubleTime <- x
                None
            | _ -> raiseMismatchType()
    
    and executeOutput value =
        match value with
        | StandardTime ->
            Number(decimal output.standardTime)
        | OverTime ->
            Number(decimal output.overTime)
        | DoubleTime ->
            Number(decimal output.doubleTime)

    and executeExpression expression =
        match expression with
        | BinaryExpression(exp1, op, exp2) ->
            executeBinaryExpression exp1 op exp2
        | UnaryExpression(op, exp1) ->
            executeUnaryExpression op exp1
        | LiteralExpression(value) ->
            executeLiteral(value)
        | OutputExpression(value) ->
           executeOutput(value)
        | CompoundExpression(_) ->
            RuntimeException "Compound expression has not yet been implemented" |> raise

    let checkPredicate (predicate : PredicateDeclaration) =
        let checkIfBoolTypeIsTrue input =
            match input with
            | Bool(true) -> true
            | _ -> false

        List.forall (executeExpression >> checkIfBoolTypeIsTrue) predicate

    for rule in program do
        if (checkPredicate (fst rule)) then
            for action in (snd rule) do
                executeExpression action |> ignore
    
    output
