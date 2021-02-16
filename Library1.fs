namespace Library1

// 1. Using visual studio create a F# Library and inside a module 'Lecture1
module Lecture1 = 

    // 2. Define functions fst, mid, lst for 3-tuple that return first, middle and last element
    let fst (a, _, _) = a
    let mid (_, b, _) = b
    let lst (_, _, c) = c

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
    //let transpose (x : float, y : float) = (y, x)
    //According to email response, I change interpretation of function meaning to addition of two vectors:
    let addition (x1 : float, y1 : float) (x2 : float, y2 : float) = (x1 + x2,  y1 + y2)

    // e) isOrtogonal v v
    let isOrthogonal (x1 : float, y1 : float) (x2 : float, y2 : float) = (x1 * x2 + y1 * y2 = 0.)

    //4. Define a record containing: Name, Salary, Department.
    // a. Department should be a Discriminated Union of IT, SALES, PR, HR
    type Department = IT | SALES | PR | HR

    type Person = { Name : string; Salary : float; Department : Department}

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
    let printIT p = 
        match p with
            | { Name = name; Salary = salary; Department = IT } -> printfn "Name %s:\t Salary: %.2f \t Department: %A" name salary IT
            | { Name = _; Salary = _; Department = _ } -> ()
    
    //Checking if it work correctly for list of employees
    let printIT_for_list = List.map printIT employees

    //assuming input is a list
    let printIT2 (inputList : Person list) = 
        for p in inputList do   
            match p.Department with
            | IT -> printfn "%A" p
            | _ -> ()

    let printIT3 (inputList : Person list) = 
        let filtered = List.filter(fun p -> match p.Department with | IT -> true | _ -> false) inputList
        List.iter(fun p -> printfn "%A" p) filtered

    let printIT4 (inputList : Person list) = 
         inputList
         |> List.filter(fun p -> match p.Department with | IT -> true | _ -> false)
         |> List.iter(fun p -> printfn "%A" p)

    //d. Define a function that sums salaries only for a specified department (argument)
    (*let salary_for_department department = 
        match department with
            | IT -> salary
            | _ -> ()*)
    
    let rec sum_salaries_for_department list =
        match list with
        | [] -> ()
        | x::xs -> sum_salaries_for_department xs

    let sumDep (l: Person list) (dep: Department) : float = 
        l
        |> List.filter(fun p -> p.Department = dep)
        |> List.sumBy(fun p -> p.Salary)

    //5. Using recursion, define a function 'removeDups : list<int> -> list<int>' that removes 
    //duplicates from the provided list. Your solution should maintain the remaining elements
    //in the same order as they were previously. It's fine for it to be slow (quadratic time).
    //(HINT: use pattern matching to deconstruct list into head and tail)
    let rec removeDups lst =
        match lst with
        | [] -> []
        | head::tail -> 
             let filtered = tail |> List.filter (fun e -> e <> head)
             head :: (removeDups filtered)

    let myList = [1;1;2;5;3;4;4;1;2;3;4;1]
    //Smart way to remove duplicates
    let distinctList = myList |> Seq.distinct |> List.ofSeq
    (*
    // Using recursive functions:
    let removeDups list1 =  
        let rev list =
            let rec loop acc = function
                | []           -> acc
                | head :: tail -> loop (head :: acc) tail
            loop [] list
        let rec alreadyExist list x =
            match list with
            | [] -> false
            | head :: tail -> if x = head then true else alreadyExist tail x
        let rec removeDupsRec list1 list2 =
            match list1 with
            | [] -> list2
            | head :: tail when alreadyExist list2 head = false -> removeDupsRec tail (head::list2)
            | _ ::tail -> removeDupsRec tail list2
        rev (removeDupsRec list1 [])
        *)
    //6. Write removeDups2(as above) using List.foldBack
    let removeDups2 lst = List.foldBack(fun elem state -> elem::List.filter(fun e -> e <> elem) state ) lst []

    (*
    let rev list =
        let rec loop acc = function
            | []           -> acc
            | head :: tail -> loop (head :: acc) tail
        loop [] list

    let times2 x = x * 2
    
    let map' f list =
        let folder acc x =
            let newElement = f x
            newElement :: acc
        List.fold folder [] list
    map' times2 [1..5] // [10; 8; 6; 4; 2]

    let reverseList list =  
        let rec loop acc = function
        | [] -> acc
        | head :: tail -> loop (head :: acc) tail
        loop [] list
    List.foldBack reverseList myList []

    let removeDups2 list1 =  
        let rec alreadyExist list x =
            match list with
            | [] -> false
            | head :: tail -> if x = head then true else alreadyExist tail x
        let rec removeDupsRec list1 list2 =
            match list1 with
            | [] -> list2
            | head :: tail when alreadyExist list2 head = false -> removeDupsRec tail (head::list2)
            | _ ::tail -> removeDupsRec tail list2
        List.foldBack removeDupsRec list1 []

        (*
        let times2 x = x * 2

        let map' f list =
            let folder acc x =
                let newElement = f x
                newElement :: acc
            List.fold folder [] list
        map' times2 [1..5] // [10; 8; 6; 4; 2]

        let map'' f list =
            let folder x acc =
                let newElement = f x
                newElement :: acc
            List.foldBack folder list []
        map'' times2 [1..5] // [2;4;6;8;10]*)
        *)

    let f x     = x * x
    let squared = List.foldBack (fun x acc -> f x :: acc) [1;2;3;4] []
    let squared_fold = List.fold (fun acc x -> f x :: acc) [] [1;2;3;4] 