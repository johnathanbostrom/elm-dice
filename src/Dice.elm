module Dice exposing (Dice(..), roll, Rules(..), andThen, plus, RollResult, RollResults(..), justResults, constant)
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
    | Custom String (Random.Generator RollResult)


type Rules = DropLowest

type RollResults = Empty
    | RollResults (List RollResult)


type alias RollResult =
        { description: String
        , value: Int
        , children: RollResults
        }

oneDieResult : String -> Int -> RollResult
oneDieResult desc val =
    RollResult desc val Empty

manyDieResult : String -> Int -> List RollResult -> RollResult
manyDieResult desc val rolls =
    RollResult desc val (RollResults rolls)


constant : Int -> Random.Generator RollResult
constant val =
    Random.constant val
    |> Random.map (oneDieResult "Constant")

plus : Random.Generator RollResult -> Random.Generator RollResult -> Random.Generator RollResult
plus diceA diceB =
    Random.map2 (\a b -> combineResults (a.description ++ " + " ++ b.description) [a,b] ) diceA diceB


dX : Int -> Random.Generator RollResult
dX sides = 
    Random.int 1 sides
    |> Random.map (oneDieResult ("D" ++ String.fromInt sides))

roll : Int -> Dice -> Random.Generator RollResult
roll numDice dieType =
    if numDice > 1 then
        toGenerator <| DicePool numDice dieType
    else if numDice == 0 then
        constant 0
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
        Custom description generator ->
            dCustom description generator

dCustom : String -> Random.Generator RollResult -> Random.Generator RollResult
dCustom desc generator =
    Random.map (\r -> {r | description = desc}) generator


dConstant : Int -> Random.Generator RollResult
dConstant val =
    oneDieResult "constant" val
    |> Random.constant

dicePool : Int -> Dice -> Random.Generator RollResult
dicePool numDice dieTypes =
    toGenerator dieTypes
    |> Random.list numDice
    |> Random.map (combineResults (String.fromInt numDice ++ " " ++ dieName dieTypes))

 
combineResults : String -> List RollResult -> RollResult
combineResults description rollResults = 
    let
        val = List.map .value rollResults |> List.foldl (+) 0
    in
        manyDieResult description val rollResults



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
    List.map .value rolls


dropLowest : RollResult -> RollResult
dropLowest result =
    case result.children of
        Empty ->
            RollResult "No Dice" 0 Empty
        RollResults rolls ->
            let
                newValue = List.map .value rolls
                    |> List.minimum
                    |> Maybe.withDefault 0
                    |> (-) result.value
            in
                {result | value = newValue }