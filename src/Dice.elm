module Dice exposing (Dice(..), roll, rollx, Rules(..))
import Random

type Dice = D4
    | D6
    | D8
    | D10
    | D12
    | D20
    | D100
    | DX Int 

type Rules = DropLowest
    | CombineResults


toGenerator : Dice -> Random.Generator Int
toGenerator dieType =
    case dieType of
        D4 ->
            dX 4
        D6 ->
            dX 6
        D8 ->
            dX 8
        D10 ->
            dX 10
        D12 ->
            dX 12
        D20 ->
            dX 20
        D100 ->
            dX 100
        DX sides->
            dX sides
       

roll : Int -> Dice -> Random.Generator (List Int)
roll numDice dieType =
    Random.list numDice (toGenerator dieType)


rollx : Int -> Dice -> List Rules -> Random.Generator (List Int)
rollx numDice dieType rules =
    roll numDice dieType
    |> handleRules rules


handleRules : List Rules -> Random.Generator (List Int) -> Random.Generator (List Int)
handleRules rules rollGenerator =
    List.map handleRule rules
    |>  List.foldl Random.map rollGenerator


handleRule : Rules -> (List Int -> List Int)
handleRule rule =
    case rule of
        DropLowest -> 
            dropLowest
        CombineResults -> 
            combineResults



dropLowest : List Int -> List Int
dropLowest rolls =
    rolls
    |> List.sort
    |> List.drop 1

combineResults : List Int -> List Int
combineResults rolls =
    List.foldl (+) 0 rolls
    |> List.singleton


dX : Int -> Random.Generator Int
dX sides =
    Random.int 1 sides
