module Dice exposing (Dice(..), roll, Rules(..), andThen, plus)
import Random

--(Int, List Int)

type Dice = D4
    | D6
    | D8
    | D10
    | D12
    | D20
    | D100
    | DX Int 
    | DCustom (Random.Generator (List Int))
    | Constant Int

type Rules = DropLowest
    | CombineResults

-- roll 2 D4 |> plus 2 D8 |> result add 1
plus : Random.Generator (List Int) -> Random.Generator (List Int) -> Random.Generator (List Int)
plus diceA diceB =
    Random.map2 (++) diceA diceB
       
roll : Int -> Dice -> Random.Generator (List Int)
roll numDice dieType =
    Random.list numDice (toGenerator dieType)


andThen : Rules -> Random.Generator (List Int) -> Random.Generator (List Int)
andThen action generator =
    Random.map (handleRule action) generator

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
        DCustom generator ->
            dCustom generator
        Constant val ->
            Random.constant val


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

dCustom : Random.Generator (List Int) -> Random.Generator Int
dCustom roller =
    Random.map cR roller 


cR : List Int -> Int
cR rolls =
    List.foldl (+) 0 rolls