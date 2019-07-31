open System
open Parser
open Interpreter

[<EntryPoint>]
let main argv =
    let employee = {Name = "Jane Smith"; HoursWorkedPerDay = [for _ in 1 .. 5 -> {HoursWorked = 10M; IsOTApproved = true}]; EmployeeType = Contractor}

    let rule = """
when
   hours worked is less than or equal to 40
then
    set ST to hours worked
end

when
   hours worked is greater than 40
then
    set ST to 40
end

when
   unapproved over time is greater than 0
then
    set OT to unapproved over time
end

when
   approved over time is less than or equal to 3
   employee type equals staff
then
    set OT to approved over time
end

when
   approved over time is greater than 3
   employee type equals staff
then
    set OT to 3
    set DT to approved over time - 3
end

when
   approved over time is greater than 0
   employee type equals contractor
then
    set DT to approved over time
end
"""

    let result =
        try
            rule
            |> parseProgram
            |> execute employee
            |> Some
        with
            | ParsingException error | RuntimeException error ->
                printfn "%s" error
                Option.None

    printfn "%A" result
    Console.ReadLine() |> ignore

    0
