namespace BlackJack

module BlackJackSmart = 
//smart way of doing that inspired by the lecture
    type Suit = Heart | Diamond | Club | Spade
    type Rank = Ace | King | Queen | Jack | Value of int
    type Card = Card of Rank * Suit        
    type HandType = Over21 | Hand of int | Blackjack

    let numericValueOfCard (Card(rank,_)) =
        match rank with
        | Ace                 -> 11
        | King | Queen | Jack -> 10
        | Value value         -> value
        
    let handFromValue sumOfCards=
        match sumOfCards with
        | 21 -> Blackjack
        | value when value > 21 -> Over21
        | value -> Hand value

    let judgeHand inputList =
        inputList
        |> List.map numericValueOfCard
        |> List.sum
        |> handFromValue
// judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (Value 3, Club) ]
// judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (King, Club) ]
// judgeHand [ Card (Value 1, Heart); Card (Value 7, Spade); Card (King, Club) ]

module BlackJack = 
    type Suit = Heart | Diamond | Club | Spade
    type Rank = Ace | King | Queen | Jack | Value of int
    type Card = Card of Rank * Suit        
    type HandType = Over21 | Hand of int | Blackjack

    let numericValueOfCard (card : Card) =
        match card with
        | Card(Ace,_)                                 -> 11
        | Card(King,_) | Card(Queen,_) | Card(Jack,_) -> 10
        | Card(Value value,_)                         -> value
       
    let rec sum listCard =
        match listCard with
        | head :: tail -> numericValueOfCard head + sum tail
        | [] -> 0

    let judgeHand inputList =
        let sumOfCards = sum inputList
        match sumOfCards with
        | 21 -> Blackjack
        | value when value > 21 -> Over21
        | value -> Hand value
    // judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (Value 3, Club) ]
    // judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (King, Club) ]
    // judgeHand [ Card (Value 1, Heart); Card (Value 7, Spade); Card (King, Club) ]

//2. Write your own definitions of List.fold and List.foldBack
//List.fold()
let rec myFold folder acc list =
    match list with
    | [] -> acc
    | head :: tail -> 
        let newAcc = folder acc head
        myFold folder newAcc tail
// myFold (+) 0 [1; 2; 3]

//List.foldBack
let rec myFoldBack folder list acc = 
    match list with
    | [] -> acc
    | head :: tail ->
        let newAcc = myFoldBack folder tail acc
        folder head newAcc
// myFoldBack (+) [1; 2; 3] 0




