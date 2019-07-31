# Timesheet DSL

## Introduction

The code in the src folder implements a simple DSL for calculating pays from timesheets.

The project was written in F# and FParsec a port of the Haskell library Parsec to F#. FParsec is a parser combinator library which allows you to write small parsers that are then combined together to form larger parsers.

## DSL

The DSL is very simple and does not contain general programming concepts like variarable declaration or functions. It's composed of Rules which have a list of predicates and a list of actions. The format of a rule is as below

```
when
   approved over time is less than or equal to 3
   employee type equals staff
then
    set OT to approved over time
end
```

Predicates exist between the 'when' and 'then' keywords. Each predicate must be placed on a seperate line and all predicates must return true for the actions contained between the 'then' and 'end' keyword to be executed.

## Code Structure

The program is split into four sections.

AST.fs contains the Abstract Syntax Tree implemented as an F# type.

Parser.fs uses FParsec to parse a string and convert it into the AST

Interpreter.fs takes the AST as input, executes it and returns the result

Programs.fs contains an example set of rules and executes the parser and interpreter on that string