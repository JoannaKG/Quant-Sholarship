namespace Library1

// 1. Using visual studio create a F# Library and inside a module 'Lecture1
module Lecture1 = 

    // 2. Define functions fst, mid, lst for 3-tuple that return first, middle and last element
    let fst (a, b, c) = a
    let mid (a, b, c) = b
    let lst (a, b, c) = c

    // 3. Let tuple (x,y) : float*float be a 2D coordinate. Define functions:
    //open System
    //Math.PI //for checking sin and cos functions 
    // a) flipX v
    let flipX (x : float, y : float) = (x, -y)

    //b) flipY v
    let flipY (x : float, y : float) = (-x, y)

    // c) rotate v angle
    let rotate (x : float, y : float) (angle : float) = 
        (
            x * cos(angle) - y * sin(angle),
            x * sin(angle) + y * cos(angle)
        )
    // d) transpose v v
    //---------ask for meaning?
    let transpose (x1 : float, y1 : float) (x2 : float, y2 : float) = (y2, x2), (y1, x1)

    // e) isOrtogonal v v
    let isOrthogonal (x1 : float, y1 : float) (x2 : float, y2 : float) = (x1 * x2 + y1 * y2 = 0.)

    //4. Define a record containing: Name, Salary, Department.
    // a. Department should be a Discriminated Union of IT, SALES, PR, HR
    type Department = 
        | IT 
        | SALES
        | PR
        | HR

    type Record = { Name : string; Salary : float; Department : Department}

    //b. Define a list containing 10 examples
    let employees = 
        [
            { Name = "Anna"; Salary = 1200.; Department = IT};
            { Name = "Beata"; Salary = 1300.; Department = SALES};
            { Name = "Cezary"; Salary = 1400.; Department = PR};
            { Name = "Dariusz"; Salary = 1500.; Department = HR};
            { Name = "Anna"; Salary = 1200.; Department = IT};
            { Name = "Ewa"; Salary = 1600.; Department = IT};
            { Name = "Filip"; Salary = 1700.; Department = SALES};
            { Name = "Grzegorz"; Salary = 1800.; Department = IT};
            { Name = "Hannna"; Salary = 1900.; Department = HR};
            { Name = "Anna"; Salary = 1200.; Department = IT};
        ]
    //c. Define a function that prints only records for IT (HINT: use pattern matching)

    let printIT record = 
        match record with
            | {Name = name; Salary = salary; Department = IT } -> printfn "Name %s:\t Salary: %.2f \t Department: %A" name salary IT
            | {Name = _; Salary = _; Department = _ } -> printfn("")
    //d. Define a function that sums salaries only for a specified department (argument)

    let sum_salaries_for_department Department =

    //Using recursion, define a function 'removeDups : list<int> -> list<int>' that removes 
    //duplicates from the provided list. Your solution should maintain the remaining lements
    //in the same order as they were previously. It's fine for it to be slow (quadratic time).
    //(HINT: use pattern matching to deconstruct list into head and tail)

    //6. Write removeDups2(as above) using List.foldBack