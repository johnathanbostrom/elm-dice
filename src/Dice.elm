module Dice exposing (Dice(..), Keep(..), RollResult, RollResults(..), Rules(..), andThen, constant, justResults, plus, roll)

import List.Extra exposing (maximumBy, minimumBy)
import Maybe
import Random


type Dice
    = D4
    | D6
    | D8
    | D10
    | D12
    | D20
    | D100
    | DX Int
    | DicePool Int Dice
    | Custom String (Random.Generator RollResult)


type Rules
    = DropLowest
    | CountSuccessesIf (Int -> Bool)
    | ExplodeIf (Int -> Bool)
    | RerollIf (Int -> Bool) Keep
      --|EndlessRerollIf (Int -> Bool) Keep
    | Do (RollResult -> Random.Generator RollResult)


type RollResults
    = Empty
    | RollResults (List RollResult)


type alias RollResult =
    { description : String
    , value : Int
    , children : RollResults
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
    Random.map2 (\a b -> combineResults (a.description ++ " + " ++ b.description) [ a, b ]) diceA diceB


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


toDie : String -> Random.Generator Int -> Dice
toDie description generator =
    Random.map (oneDieResult description) generator
        |> Custom description


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

        DX sides ->
            dX sides

        DicePool numDice dieTypes ->
            dicePool numDice dieTypes

        Custom description generator ->
            dCustom description generator


dCustom : String -> Random.Generator RollResult -> Random.Generator RollResult
dCustom desc generator =
    Random.map (\r -> { r | description = desc }) generator


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
        val =
            List.map .value rollResults |> List.foldl (+) 0
    in
    manyDieResult description val rollResults


andThen : Rules -> Random.Generator RollResult -> Random.Generator RollResult
andThen rule rolls =
    case rule of
        DropLowest ->
            Random.map dropLowest rolls

        CountSuccessesIf test ->
            Random.map (countSuccessesIf test) rolls

        ExplodeIf test ->
            explodeIf test rolls

        RerollIf test keep ->
            rerollIf test keep rolls

        Do action ->
            Random.andThen action rolls


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

        DX sides ->
            "D" ++ String.fromInt sides

        DicePool numDice dT ->
            String.fromInt numDice ++ "D" ++ dieName dT

        Custom description _ ->
            description


justResults : List RollResult -> List Int
justResults rolls =
    List.map .value rolls



--andThen helpers
--TODO: reroll keep high, low.  How many times?


type Keep
    = Low
    | High
    | New
    | KeepCustom (List RollResult -> RollResult)


rerollIf : (Int -> Bool) -> Keep -> Random.Generator RollResult -> Random.Generator RollResult
rerollIf test keep generator =
    generator
        |> Random.andThen (reroll generator test)
        |> Random.map (combineResults "Reroll")
        |> Random.map (chooseReroll keep)


reroll : Random.Generator RollResult -> (Int -> Bool) -> RollResult -> Random.Generator (List RollResult)
reroll generator test rollResult =
    if test rollResult.value then
        Random.constant rollResult
            |> Random.list 1
            |> Random.map2 (::) generator

    else
        Random.constant rollResult
            |> Random.list 1


chooseReroll : Keep -> RollResult -> RollResult
chooseReroll keep rollResult =
    case rollResult.children of
        Empty ->
            rollResult

        RollResults rolls ->
            let
                keptRoll =
                    case keep of
                        High ->
                            List.map .value rolls
                                |> List.maximum
                                |> Maybe.withDefault 0

                        Low ->
                            List.map .value rolls
                                |> List.minimum
                                |> Maybe.withDefault 0

                        New ->
                            List.map .value rolls
                                |> List.head
                                |> Maybe.withDefault 0

                        KeepCustom action ->
                            (action rolls).value
            in
            { rollResult | value = keptRoll }


explodeIf : (Int -> Bool) -> Random.Generator RollResult -> Random.Generator RollResult
explodeIf test generator =
    generator
        |> Random.andThen (explode generator test)
        |> Random.map (combineResults "Explode!")


explode : Random.Generator RollResult -> (Int -> Bool) -> RollResult -> Random.Generator (List RollResult)
explode generator test rollResult =
    if test rollResult.value then
        Random.constant rollResult
            |> Random.list 1
            |> Random.map2 (++) (generator |> Random.andThen (explode generator test))

    else
        Random.constant rollResult
            |> Random.list 1


dropLowest : RollResult -> RollResult
dropLowest result =
    case result.children of
        Empty ->
            { result | value = 0 }

        RollResults rolls ->
            let
                newValue =
                    List.map .value rolls
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> (-) result.value
            in
            { result | value = newValue }


countSuccessesIf : (Int -> Bool) -> RollResult -> RollResult
countSuccessesIf test rollResult =
    let
        successes =
            case rollResult.children of
                Empty ->
                    succeedVal test rollResult

                RollResults rolls ->
                    List.map (succeedVal test) rolls
                        |> List.sum
    in
    { rollResult | value = successes }


succeedVal : (Int -> Bool) -> RollResult -> Int
succeedVal test rollResult =
    if test rollResult.value then
        1

    else
        0
