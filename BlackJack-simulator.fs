namespace BlackJack

module BlackJack = 
    type Suit = Heart | Diamond | Club | Spade
    type Rank = Ace | King | Queen | Jack | Value of int
    type Card = Card of Rank * Suit        
    type HandType = Over21 | Hand of int | Blackjack

    let numericValueOfCard (card : Card) =
        match card with
        | Card(Ace,_)           -> 11
        | Card(King,_)          -> 10
        | Card(Queen,_)         -> 10
        | Card(Jack,_)          -> 10
        | Card(Value value,_)   -> value
        
    let rec sum listCard =
        match listCard with
        | head :: tail -> numericValueOfCard head + sum tail
        | [] -> 0

    let judgeHand inputList =
        let sumOfCards = sum inputList
        match sumOfCards with
        | 21 -> Blackjack
        | value when value > 21 -> Over21
        | value when value < 21 -> Hand value

    judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (Value 3, Club) ]
    judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (King, Club) ]
    judgeHand [ Card (Value 1, Heart); Card (Value 7, Spade); Card (King, Club) ]

//2. Write your own definitions of List.fold and List.foldBack
//TODO