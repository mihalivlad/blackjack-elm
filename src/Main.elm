-----------------------
-- Mihali Vlad
-- 10.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( Model (newCard::model.hand) (List.filter (\x -> x /= newCard) model.deck) model.showDeck
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      ( Model model.hand model.deck (not model.showDeck)
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

firstOrZero : List Int -> Int
firstOrZero l =
    case l of
        x::_->x
        []->0

sums : List Card -> List Int -> List Int
sums cards l =
    case cards of
        (Card Ace _)::xs -> sums xs (List.append (List.map (\x -> x + 1) l) (List.map (\x -> x + 11) l))
        card::xs-> sums xs (List.map (\x-> x + (firstOrZero (cardValue card))) l)
        []-> List.sort l


calculateScore : List Card -> Int
calculateScore cards =
    let
        l = sums cards [0]
        before21 = List.reverse (List.filter (\x -> x<=21) l)
    in
       case before21 of
           x::_ -> x
           [] -> firstOrZero l

firstCard: List Card -> Card
firstCard cards =
    case cards of
        x::_->x
        []-> Card Ace Clubs

viewAllCards : List Card -> List (Html msg)
viewAllCards l =
    case l of
        x::xs -> (viewCard x)::(viewAllCards xs)
        [] -> [div[][]]

showDeck : Model -> List (Html msg)
showDeck model =
    if model.showDeck then (viewAllCards model.deck) else []

showWinLose : Model -> List (Html msg)
showWinLose model =
    if calculateScore model.hand == 21 then
       [text ("WIN")]
    else if calculateScore model.hand > 21 then
       [text ("LOSE")]
    else
        []

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
  in
    div []
      [ button [ onClick Draw, disabled (calculateScore model.hand >= 21)] [ text "Draw" ]
                , button [ onClick ToogleDeck ] [ text "Toogle Deck" ]
                , div [] (showDeck model)
                , div [] (viewAllCards model.hand)
                , div [] [text ("score : " ++ String.fromInt (calculateScore model.hand))]
                , div [style "font-size" "20em"] (showWinLose model)
                ]