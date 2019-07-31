module AST

type Program = RuleDeclaration list
and RuleDeclaration = PredicateDeclaration * ActionDeclaration
and PredicateDeclaration = Statement list
and ActionDeclaration = Statement list
and Statement = Expression
and Expression =
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | LiteralExpression of Literal
    | OutputExpression of OutputExpression
    | CompoundExpression of CompoundExpression
and CompoundExpression =
    | SumExpression of Statement list * Statement
and OutputExpression =
    | StandardTime
    | OverTime
    | DoubleTime
and BinaryOperator = 
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreaterEqual
    | Greater
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    | Assignment
    override x.ToString() =
        match x with
        | Add            -> "+"
        | Subtract       -> "-"
        | Multiply       -> "*"
        | Divide         -> "/"
        | Modulus        -> "%"
        | ConditionalOr  -> "or"
        | Equal          -> "equals"
        | NotEqual       -> "is not equal to"
        | LessEqual      -> "is less than or equal to"
        | Less           -> "is less than"
        | GreaterEqual   -> "is greater than or equal"
        | Greater        -> "is greater than"
        | ConditionalAnd -> "and"
        | Assignment     -> "="
and UnaryOperator =
    | LogicalNegate
    | SetStandardTime
    | SetOverTime
    | SetDoubleTime
    override x.ToString() =
        match x with
        | LogicalNegate -> "not"
        | SetStandardTime -> "set ST to"
        | SetOverTime -> "set OT to"
        | SetDoubleTime -> "set DT to"
and Literal =
    | BoolLiteral of bool
    | NumberLiteral of decimal
    | StringLiteral of string
    | HoursWorked
    | ApprovedOverTime
    | UnapprovedOverTime
    | EmployeeType
    | Contractor
    | Staff
    override x.ToString() =
        match x with
        | BoolLiteral(b)     -> b.ToString()
        | NumberLiteral(n)   -> n.ToString()
        | StringLiteral(f)   -> f.ToString()
        | HoursWorked        -> "hours worked"
        | ApprovedOverTime   -> "approved over time"
        | UnapprovedOverTime -> "unapproved over time"
        | EmployeeType       -> "employee type"
        | Contractor         -> "contractor"
        | Staff              -> "staff"
