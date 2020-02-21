module Dice exposing (Dice(..), roll, Rules(..), andThen, plus, RollResult(..), DieResult, justResults)
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
    | DCustom (Random.Generator (List RollResult))
    | Constant Int

type Rules = DropLowest


type RollResult = OneDie DieResult
    | MultipleDice DiceResult


type alias DiceResult =
    { value: Int
    , rolls: List RollResult
    }

type alias DieResult =
    { dieType: String
    , value: Int
    }

plus : Random.Generator (List RollResult) -> Random.Generator (List RollResult) -> Random.Generator (List RollResult)
plus diceA diceB =
    Random.map2 (++) diceA diceB

dX : Int -> Random.Generator RollResult
dX sides = 
    Random.int 1 sides
    |> Random.map (DieResult ("D" ++ String.fromInt sides))
    |> Random.map OneDie

roll : Int -> Dice -> Random.Generator (List RollResult)
roll numDice dieType =
    Random.list numDice (toGenerator dieType)

toGenerator : Dice -> Random.Generator RollResult
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
            Random.constant (OneDie (DieResult "constant" val))

dCustom : Random.Generator (List RollResult) -> Random.Generator RollResult
dCustom roller =
    Random.map combineResults roller
    |> Random.map MultipleDice


combineResults : List RollResult -> DiceResult
combineResults rollResults = 
    let
        val = List.map (\d -> getRollValue d) rollResults |> List.foldl (+) 0
    in
        DiceResult val rollResults

getRollValue : RollResult -> Int
getRollValue rollResult =
    case rollResult of
        OneDie d -> d.value
        MultipleDice m -> m.value


andThen : Rules -> Random.Generator (List RollResult) -> Random.Generator (List RollResult)
andThen action generator =
    Random.map (handleRule action) generator


dieName : Dice -> String
dieName dieType =
    case dieType of
        D4 ->
            "D4"
        D6 ->
            "D6"
        D8 ->
            "D8"
        D10 ->
            "D10"
        D12 ->
            "D12"
        D20 ->
            "D20"
        D100 ->
            "D100"
        DX sides->
            "D" ++ (String.fromInt sides)
        DCustom generator ->
            "Custom"
        Constant val ->
            "Constant: " ++ (String.fromInt val)


handleRules : List Rules -> Random.Generator (List RollResult) -> Random.Generator (List RollResult)
handleRules rules rollGenerator =
    List.map handleRule rules
    |>  List.foldl Random.map rollGenerator


handleRule : Rules -> (List RollResult -> List RollResult)
handleRule rule =
    case rule of
        DropLowest -> 
            dropLowest

justResults : List RollResult -> List Int
justResults rolls =
    List.map justResult rolls

justResult : RollResult -> Int
justResult rollResult =
    case rollResult of
        OneDie d -> 
            d.value
        MultipleDice m -> 
            m.value

dropLowest : List RollResult -> List RollResult
dropLowest rolls =
    rolls
    |> List.sortBy getRollValue
    |> List.drop 1