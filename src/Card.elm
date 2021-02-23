-----------------------
-- Mihali Vlad
-- 10.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck)

import Html exposing (..)
import Html.Attributes exposing (style)

type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type Card = Card  Face Suit

faceToString : Face -> String
faceToString n =
     case n of
         Ace -> "Ace"
         Two -> "Two"
         Three -> "Three"
         Four -> "Four"
         Five -> "Five"
         Six -> "Six"
         Seven -> "Seven"
         Eight -> "Eight"
         Nine -> "Nine"
         Ten -> "Ten"
         Jack -> "Jack"
         Queen -> "Queen"
         _ -> "King"

suitToString : Suit -> String
suitToString suit =
     case suit of
         Clubs -> "Clubs"
         Diamonds -> "Diamonds"
         Hearts -> "Hearts"
         _ -> "Spades"

--input
--cardToString (Card Ace Spades)
cardToString : Card -> String
cardToString (Card face suit) =
    faceToString face ++ " of " ++ suitToString suit

cardValue : Card -> List Int
cardValue card =
    case card of
        Card Ace _ -> [1,11]
        Card Two _ -> [2]
        Card Three _ -> [3]
        Card Four _ -> [4]
        Card Five _ -> [5]
        Card Six _ -> [6]
        Card Seven _ -> [7]
        Card Eight _ -> [8]
        Card Nine _ -> [9]
        Card Ten _ -> [10]
        Card Jack _ -> [10]
        Card Queen _ -> [10]
        Card King _ -> [10]

fullSuit: Suit -> List Card
fullSuit suit = [ Card Ace  suit, Card Two suit, Card Three suit, Card Four suit, Card Five suit, Card Six suit, Card Seven suit, Card Eight suit, Card Nine suit, Card Ten suit, Card Jack suit, Card Queen suit, Card King suit ]
deck: List Card
deck =
    List.append (List.append (fullSuit Clubs) (fullSuit Diamonds)) (List.append (fullSuit Hearts) (fullSuit Spades))


{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card face suit)  =
   let
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode (Card face suit)
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString (Card face suit))]
     ]