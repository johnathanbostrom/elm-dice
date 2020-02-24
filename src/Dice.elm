module Dice exposing (Dice(..), roll, Rules(..), andThen, plus, RollResult(..), DieResult, justResults)
import Random
import Maybe

type Dice = D4
    | D6
    | D8
    | D10
    | D12
    | D20
    | D100
    | DX Int 
    | DicePool Int Dice
    | Constant Int
    | Custom String (Random.Generator RollResult)


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

roll : Int -> Dice -> Random.Generator RollResult
roll numDice dieType =
    if numDice > 1 then
        toGenerator <| DicePool numDice dieType
    else if numDice == 0 then
        toGenerator <| Constant 0
    else 
        toGenerator dieType


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
        DicePool numDice dieTypes ->
            dicePool numDice dieTypes
        Constant val ->
            dConstant val
        Custom _ generator ->
            generator

dConstant : Int -> Random.Generator RollResult
dConstant val =
    OneDie (DieResult "constant" val)
    |> Random.constant

dicePool : Int -> Dice -> Random.Generator RollResult
dicePool numDice dieTypes =
    toGenerator dieTypes
    |> Random.list numDice
    |> Random.map combineResults
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


andThen : Rules -> Random.Generator RollResult -> Random.Generator RollResult
andThen rule rolls =
    rolls 
    |> Random.map (handleRule rule)


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
            "D" ++ String.fromInt sides
        Constant val ->
            "Constant: " ++ String.fromInt val
        DicePool numDice dT ->
            String.fromInt numDice ++ "D" ++ dieName dT
        Custom description _ ->
            description


handleRule : Rules -> RollResult -> RollResult
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


dropLowest : RollResult -> RollResult
dropLowest rollResult =
    case rollResult of
        OneDie d ->
            OneDie {d | value = 2 }
        MultipleDice m ->
            let
                newValue = List.map getRollValue m.rolls
                    |> List.minimum
                    |> Maybe.withDefault 0
                    |> (-) m.value
            in
                MultipleDice {m | value = newValue }