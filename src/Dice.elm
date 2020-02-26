module Dice exposing
    ( roll, Dice(..), RollResult, ChildrenRolls(..)
    , andThen, Rules(..), Keep(..), plus
    , toDie
    )

{-| A Dice Roller Package based on elm/random that allows you to build customizable dice rolling functions in a readable way.


# Definition

@docs roll, Dice, RollResult, ChildrenRolls


# Combining and Transforming Rolls

@docs andThen, Rules, Keep, plus


# Common Helpers

@docs toDie

-}

import List.Extra exposing (maximumBy, minimumBy)
import Maybe
import Random


{-| Represents a Die or pool of dice. Most common dice are built in, but you can create your own using DX, Custom, or Constant.

    -- A 5 sided die.
    d5 = DX 5

    -- A custom die with values from 3 to 12. see toDie
    Random.int 3 12
    |> toDie
    |> Custom "D3To12"

    -- A Constant Value
    always4 = Constant "Four" 4

-}
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
    | Constant String Int


{-| Rules for chaining together rolls and building more complex die rolls. You can create custom rules with Do.
    --rolls four six sided dice, then drops the lowest die
       roll 3 D6 |> andThen DropLowest

    --rolls eight ten sided dice, then counts how many came up 8 or higher.
        roll 8 D10 |> andThen CountSuccessesIf (\r -> r > 7)

    --defines a ten sided die that "explodes" on a 10.  This die will keep rolling for as long as it rolls a ten.
        explodingDie = roll 1 D10
            |> andThen ExplodeIf ((==) 10)

    --rolls eight exploding dice, then counts how many came up 8 or higher.
        roll 5 explodingDie
        |> andThen CountSuccessesIf (\r -> r > 7)

    --rolls 2 six sided dice. Then rerolls any ones or twos, keeping the new result even if lower.  For instance, on a roll of 5 and 2, the two would be rerolled.
    --If the new roll came up a 1, the result would be 6 (5 and 1).
        roll 2 D6
        |> andThen RerollIf (\r -> r < 2)

-}
type Rules
    = DropLowest
    | CountSuccessesIf (Int -> Bool)
    | ExplodeIf (Int -> Bool)
    | RerollIf (Int -> Bool) Keep
    | Do (RollResult -> Random.Generator RollResult)


{-| represents the children of a RollResult.
-}
type ChildrenRolls
    = Empty
    | RollResults (List RollResult)


{-| Represents how we decide when choosing between multiple die results. see RerollIf
-}
type Keep
    = Low
    | High
    | New
    | KeepCustom (List RollResult -> RollResult)


{-| Represents the results of a roll.

    --possible result of `roll 1 D6`:
    { descripion = "1D6"
    , value = 4
    , children = Empty
    }

    --possible result of `roll 2 D6`:
    { description = "3D6"
    , value = 8
    , children =
        RollResults
            [ { description = "1D6", value = 3, children = Empty }
            , { description = "1D6", value = 5, children = Empty }
            ]
    }

-}
type alias RollResult =
    { description : String
    , value : Int
    , children : ChildrenRolls
    }


{-| make a Random.Generator for a roll of n dice.

    roll 3 D6

-}
roll : Int -> Dice -> Random.Generator RollResult
roll numDice dieType =
    if numDice > 1 then
        toGenerator <| DicePool numDice dieType

    else if numDice <= 0 then
        constant 0

    else
        toGenerator dieType


{-| combine the results of two rolls. The value of the resulting roll will be the sum of the values of those two rolls.

    d6PlusD4 =
        roll 1 D6
            |> plus (roll 1 D4)
                --one possible result:
                { description = "1D6 + 1D4"
                , value = 8
                , children =
                    RollResults
                        [ { description = "1D6", value = 6, children = Empty }
                        , { description = "1D4", value = 2, children = Empty }
                        ]
                }

-}
plus : Random.Generator RollResult -> Random.Generator RollResult -> Random.Generator RollResult
plus diceA diceB =
    Random.map2 (\a b -> combineResults (a.description ++ " + " ++ b.description) [ a, b ]) diceA diceB


{-| used to apply transformations to RollResult generators. Some common rolling rules are defined, and you can create your own with [`Do`](#Rules). -}
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


{-| Converts a Random.Generator Int into a Random.Generator RollResult.
-}
toDie : String -> Random.Generator Int -> Random.Generator RollResult
toDie description generator =
    Random.map (oneDieResult description) generator



-- Local Functions


oneDieResult : String -> Int -> RollResult
oneDieResult desc val =
    RollResult desc val Empty


manyDieResult : String -> Int -> List RollResult -> RollResult
manyDieResult desc val rolls =
    RollResult desc val (RollResults rolls)


dX : Int -> Random.Generator RollResult
dX sides =
    Random.int 1 sides
        |> Random.map (oneDieResult ("D" ++ String.fromInt sides))


constant : Int -> Random.Generator RollResult
constant val =
    Random.constant val
        |> Random.map (oneDieResult "Constant")


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

        Constant _ val ->
            constant val


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

        Constant description _ ->
            description


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
