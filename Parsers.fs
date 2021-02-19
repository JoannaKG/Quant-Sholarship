// Lets define simple library of parsers combinators (https://en.wikipedia.org/wiki/Parser_combinator)
// Main building blocks is a function char seq -> option ('t * char seq)

type Parser<'t> = seq<char> -> option<'t * seq<char>>

// Let's decode the signature: 
//    1. "->" indicates that type of the parser is a function
//    2. Left handside of "->" is the type of argument of the parser function. It's: seq<char>. So the parser will consume a sequence of characters
//    3. Right handside of "->" is the type of the result. It's an option, so we can either get some result of underlying type (the 't * seq<char>) or get None.
//       Optionality of the result corresponds to the success or failure of the Parser processing of the given input
//    4. The inner type of result : 't * seq<char> is a tuple holding result of type 't and the rest of the input (the part that was not consume by the parser)

// NOTE: Parser<'t> result is an option!. You can use Option.map and option.bind on it
// NOTE: Parser<'t> is function. you Can pipe input parameter into Parser: "fooBar" |> fooBarParser. Or compose it with >> : fooBarParser >> Option.map ( ...) 
// Let's define example:
module Parsers = 
    let pChar c (input: seq<char>) =       
        if Seq.isEmpty input |> not && Seq.head input = c
        then Some (c, Seq.skip 1 input)             
        else None

    // function pChar eats given character c from the head of the input
    // NOTE: The result of partially applied function Parsers.char 'a' is of type seq<char> -> option<char * seq<char>> which is Parser<char>!
    // NOTE: we could define it equvalently as 
    let char2 c : Parser<char> =  
        fun (input: seq<char>) ->      
            if Seq.isEmpty input |> not && Seq.head input = c
            then Some (c, Seq.skip 1 input)             
            else None
    // We can execute it by (we benefit that string is a seq<char>):
    //Parsers.pChar 'f' "fooBar";;
    //    val it : (char * seq<char>) option = Some ('f', seq ['o'; 'o'; 'B'; 'a'; ...])

// Excercise 1. 
//    a. Define function 'pAny' that eats single (any) character from the input Parser<char>
    let pAny (input: seq<char>) =       
        if Seq.isEmpty input |> not
        then Some (Seq.head input, Seq.skip 1 input)             
        else None
    //Parsers.pAny "booBar";;

//    b. Define function 'pDigit' that parse single digit from the input and return Parser<int>
    let pDigit (input: seq<char>) =    
        let inline charToInt c = int c - int '0'
        if Seq.isEmpty input |> not && (List.contains (Seq.head input) ['0'..'9'])
        //then Some ((Seq.head input) |> sprintf "%c" |> int, Seq.skip 1 input) 
        then Some ((Seq.head input) |> charToInt, Seq.skip 1 input)             
        else None
    //Parsers.pDigit "0fooBar";;

//    c. Define function 'pSpace' that eats all spaces from the begining of the input
    let pSpace (input: seq<char>) =       
        if Seq.isEmpty input |> not && Seq.head input = ' '
        then Some (Seq.takeWhile (fun c -> c = ' ' ) input, Seq.skipWhile (fun c -> c = ' ' ) input)             
        else None
    //Parsers.pSpace "   foo Bar";;
//    d. Define function 'pWord w' that tries to parse w at the begining of the input  
    let pWord w (input: seq<char>) =    
        let length = w |> Seq.length
        let zipped = Seq.zip w input
        if Seq.length zipped = length && zipped |> Seq.forall (fun (a,b) -> a=b)
        then Some (w, Seq.skip length input)             
        else None
    Parsers.pWord "foo" "foo Bar";; 

// Till now we defined a few primitive parsers. Next step is to define higher order function that are able to combine them (combinators)
// Here is example

module Combinators = 
    //returns result of first parser if the whole input is sucessfully parsed by the combinator
    let combineL (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  //run the first parser ..
        | Some (result, restOfInput) -> // ... and check the result
            match p2 restOfInput with // if successed run the second parser
            | Some (_, restOfInput2) -> Some (result, restOfInput2) // and if success return result of first parser
            | None -> None // second parser failed
        | _ -> None // first parser failed 

    // combineL combines two parsers and if both succeded returns result of the first one
    // here's usage example:
    //    > let result: Parser<char> = Combinators.combineL (Parsers.pChar 'a') (Parsers.pChar 'b');;
    //    > result "bb";;
    //    val it : (char * seq<char>) option = None
    //    > result "abc";;
    //    val it : (char * seq<char>) option = Some ('a', seq [])  // NOTE: value here is only 'a' but whole input was eaten by the combined parsers
    //    > result "ac";;
    //    val it : (char * seq<char>) option = None

// Excercise 2.
//    a. Define 'combineR p1 p2' similar to combineL but return result of second parser
    let combineR (p1: Parser<'t1>) (p2: Parser<'t2>) (input: seq<char>) =
        match p1 input with  //run the first parser ..
        | Some (_, restOfInput) -> // ... and check the result
            match p2 restOfInput with // if successed run the second parser
            | Some (result, restOfInput2) -> Some (result, restOfInput2) // and if success return result of first parser
            | None -> None // second parser failed
        | _ -> None // first parser failed 
    //    > let result: Parser<char> = Combinators.combineR (Parsers.pChar 'a') (Parsers.pChar 'b');;
    //    > result "bb";;
    //    val it : (char * seq<char>) option = None
    //    > result "abc";;
    //    val it : (char * seq<char>) option = Some ('b', seq [])  // NOTE: value here is only 'a' but whole input was eaten by the combined parsers
    //    > result "ac";;
    //    val it : (char * seq<char>) option = None

    //    b. Try to rewrite combineR using Option.bind. Hint: Did you use >> ? .
    let combineRBind (p1: Parser<'t1>) (p2: Parser<'t2>) =
        p1 >> Option.bind (snd >> p2)
    //    let result: Parser<char> = combineRBind (Parsers.pChar 'a') (Parsers.pChar 'b');;
    //    result "abc";;

    //    c. Define 'combine p1 p2' that produce tuple of result from both parsers
    let combine p1 p2 input =
        match p1 input with  //run the first parser ..
        | Some (result, restOfInput) -> // ... and check the result
            match p2 restOfInput with // if successed run the second parser
            | Some (result2, restOfInput2) -> Some((result, result2), restOfInput2) 
            | None -> None
        | _ -> None 
//    let result = Combinators.combine (Parsers.pChar 'a') (Parsers.pChar 'b') "abcd";;

//    d. Is it possible to rewrite comblineL and combine the same way combineR was in part b.?
    let combineLBind (p1: Parser<'t1>) (p2: Parser<'t2>) =
        let runP (result, input) = p2 input |> Option.map(fun (_, restOfInput) -> (result, restOfInput))
        p1 >> Option.bind runP
    let combineBind (p1: Parser<'t1>) (p2: Parser<'t2>) =
        let runP (result, input) = p2 input |> Option.map(fun (result2, restOfInput) -> ((result,result2), restOfInput))
        p1 >> Option.bind runP
//    let result = combineLBind (Parsers.pChar 'a') (Parsers.pChar 'b') "abcd";;
//    e. Define 'orP p1 p2' which will atempt the first parser and if fail attempt second. or should fail if both p1 and p2 fails 
    let orP p1 p2 input =
        match p1 input with  //run the first parser ..
        | Some _ as result -> result // ... and check the result
        | None -> p2 input
//    let result = orP (Parsers.pChar 'a') (Parsers.pChar 'b') "bbcd";;

//    d. Define 'map p f' which will map the result of a praser with function f: Hint: Use Option.map
    let map (p: Parser<'t>) f = 
        p >> Option.map( fun(result, restOfInput) -> (f result, restOfInput))
    
/// For clarity let's define set of infix operators for the methods defined above
module Operators = 
    open Parsers
    open Combinators
    let (.>>) : Parser<'a> -> Parser<'b> -> Parser<'a> = combineL
    let (>>.) : Parser<'a> -> Parser<'b> -> Parser<'b> = combineR 
    let (.>>.) : Parser<'a> -> Parser<'b> -> Parser<'a*'b> = combine
    let (|>>) : Parser<'a> -> ('a -> 'b) -> Parser<'b> = map
    let (<|>) : Parser<'a> -> Parser<'a> -> Parser<'a> = orP

// here is an example of operator usage = 
    let addParser : Parser<int> = pDigit .>> pChar '+' .>>. pDigit |>> fun (x,y) -> x + y
// First we parse for a digit followed by a '+' char. Since we use .>> (combineL) the result holds only the parsed digit.
// Next we parse another digit and glue the result into tuple of ints with .>>. (combine combinator). 
// Finally the int tuple is maped through sum function. Let's try to execute the parser:
//        > Operators.addParser "1+9";;
//        val it : (int * seq<char>) option = Some (10, seq [])   

// Excercise 3.
// here is simple gramma of arithmentic expression:
// < expression > ::= < term > + < expression > 
//                  | < term > - < expression > 
//                  | < term >
// < term > ::= < factor > * < term > 
//            | < factor > / < term > 
//            | < factor >
// < factor > ::= (< expression >) | < digit > 
// Write parser that computes the result of arithmetic expression string.
module Expressions = 
    open Parsers
    open Operators

    // here is module with some helper parsers and functions
    [<AutoOpen>]
    module helpers = 
        let pPlus: Parser<_> = pChar '+'
        let pMinus: Parser<_> = pChar '-'
        let pMult: Parser<_> = pChar '*'
        let pDiv: Parser<_> = pChar '/'
        let pOpen: Parser<_> = pChar '('
        let pClose: Parser<_> = pChar ')'


        let sum (a,b) = a+b;     
        let minus (a,b) = a-b;
        let mul (a,b) = a*b;
        let div (a,b) = a/b;
    
    // to help you start of I prepare scaffoling for the solution: 
    let rec expression (input:seq<char>) : option<int * seq<char>>= 
        let pExpression = (term .>> pPlus .>>. expression |>> sum) // the case for summing term with an expression. It coresponds to < term > + < expression > part of the grammar 
                          <|> (term .>> pMinus .>>. expression |>> minus)// TODO: put a case for a term - expression it should correspond to < term > - < expression > 
                          <|> term // TODO: put a case for single term < term >
        input |> pExpression // here we trigger parsers on the input

    and term (input:seq<char>) : option<int * seq<char>> = 
        let pTerm = (factor .>> pMult .>>. term |>> mul)// TODO: mutliplication 
                    <|> (factor .>> pDiv .>>. term |>> div )// TODO: or divide 
                    <|> factor // TODO: or factor 
        input |> pTerm

    and factor  (input:seq<char>) : option<int * seq<char>>  =  
        let pFactor = (pOpen >>. expression .>> pClose) <|> pDigit// TODO: expression in parenthesis or a digit
        input |> pFactor

    // Excercise 4:Write some test for different expresions 
    let checkSum = Expressions.expression "6+5";;   //OK
    let checkMinus = Expressions.expression "6-5";; //OK
    let checkMult = Expressions.expression "6*5";;  //OK
    let checkDiv = Expressions.expression "4/2";;   //OK
    let checkDiv2 = Expressions.expression "(3+1)/2";;  //OK
    let checkDiv2 = Expressions.expression "( 3 + 1 )/2";;  //OK
    let checkSumMin = Expressions.expression "6+5-4";; //OK
    let checkMulSum = Expressions.expression "5*(4+1)";; //OK
    let checkMulMulDiv = Expressions.expression "(2*2)/4";; //OK

    let checkMulMulDiv2 = Expressions.expression "2*2/2";; //WRONG!!!, it=0 because parser goes from right to left
    let checkSum2 = Expressions.expression "60+5";;  //WRONG!!!, it = 6 because paser works for single char
    let checkDiv = Expressions.expression "30/2";;  //WRONG!!!, it = 3

    // Excercise 5: The parser above is sensitive to whitespaces. "5 + 6" Won't parse. Extent the parse so it ignores whitespaces.
    let expressionWithoutWhitespaces : Parser<int> = Seq.filter (System.Char.IsWhiteSpace >> not) >> Expressions.expression 

    let checkDiv2 = expressionWithoutWhitespaces "8 / 2";;
    // Excercise 6. There is a bug in the parser above. "5+7foobar" is valid expression. Write parser endInput : Parser<_> which success on empty input. Create safeExpression which combines 
    // expression and endInput so that "5+7foobar" is no longer valid input.
    let safeExpression : Parser<_>= 
        let endInput input =
            if Seq.isEmpty input
            then Some((), Seq.empty)
            else None
        Expressions.expression .>> endInput

    let checkSave = safeExpression "5+7foobar";;