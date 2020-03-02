module Dice exposing
    ( roll, Dice(..), RollResult, ChildrenRolls(..)
    , dropLowest, countSuccessesIf, explodeIf, rerollIf, andThen, Keep(..), plus
    , toRollResult
    )

{-| A Dice Roller Package based on elm/random that allows you to build customizable dice rolling functions in a readable way.


# Definition

@docs roll, Dice, RollResult, ChildrenRolls


# Combining and Transforming Rolls

@docs dropLowest, countSuccessesIf, explodeIf, rerollIf, andThen, Keep, plus


# Common Helpers

@docs toRollResult

-}

import List.Extra exposing (count, maximumBy, minimumBy)
import Maybe
import Random


{-| Represents a Die or pool of dice. Most common dice are built in, but you can also create your own.

    -- A 5 sided die.
    d5 =
        DX 5

    -- A custom die
    customDie =
        CustomDie "EvenRoller" [ 2, 2, 2, 2, 4, 4, 4, 6, 6, 8 ]

    -- A weighted die. Effectively the same as the customDie above.
    weightedDie =
        WeightedDie "EvenRoller"
            [ ( 4, 2 )
            , ( 3, 4 )
            , ( 2, 6 )
            , ( 1, 8 )
            ]

    -- A Constant Value
    always4 =
        Constant "Four" 4

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
    | DicePool String (Random.Generator RollResult)
    | CustomDie String (List Int)
    | WeightedDie String (List ( Float, Int ))
    | Constant String Int


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
        toGenerator dieType
            |> Random.list numDice
            |> Random.map (combineResults (String.fromInt numDice ++ " " ++ dieName dieType))

    else if numDice <= 0 then
        constant "No Dice" 0

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


{-| used to apply transformations to RollResult generators.
-}
andThen : (RollResult -> Random.Generator RollResult) -> Random.Generator RollResult -> Random.Generator RollResult
andThen generator result =
    Random.andThen generator result


{-| Recalculates the value of a RollResult by dropping the lowest value of its children.
If the RollResult had no children (because it was the result of a single die roll or constant value),
Then dropLowest will set the value to zero.

    statGenerator =
        roll 4 D6 |> dropLowest

-}
dropLowest : Random.Generator RollResult -> Random.Generator RollResult
dropLowest rolls =
    Random.map dropLowestRoll rolls


{-| Recalculates the value of a RollResult by counting the number of children that pass the test.

        roll 3 D10
            |> countSuccessesIf (\r -> r > 7)

-}
countSuccessesIf : (RollResult -> Bool) -> Random.Generator RollResult -> Random.Generator RollResult
countSuccessesIf test generator =
    Random.map (successes test) generator


{-| "explodes" a RollResult. An exploding die will keep rolling as long as it satisfies the predicate. For instance, if you roll a 10 on the following D10, you will roll again and add the rolls together. If your reroll is another D10, you repeat this process.

    --defines a ten sided die that "explodes" on a 10.
    explodingDie =
        roll 1 D10
            |> andThen ExplodeIf ((==) 10)

Currently, all dice are limited to 100 explosions.

-}
explodeIf : (RollResult -> Bool) -> Random.Generator RollResult -> Random.Generator RollResult
explodeIf test generator =
    generator
        |> Random.andThen (explode generator test)
        |> Random.map (combineResults "Explode!")


{-| Rerolls a RollResult if it satisfies the given predicate.
The value of one roll will be kept using the Keep rules specified.

    -- rerolls any ones or twos, keeping the new result even if lower.
        roll 2 D6
        |> andThen RerollIf (\r -> r.value < 2) New

-}
rerollIf : (RollResult -> Bool) -> Keep -> Random.Generator RollResult -> Random.Generator RollResult
rerollIf test keep generator =
    generator
        |> Random.andThen (reroll generator test)
        |> Random.map (combineResults "Reroll")
        |> Random.map (chooseReroll keep)


{-| Converts a Random.Generator Int into a Random.Generator RollResult.
-}
toRollResult : String -> Random.Generator Int -> Random.Generator RollResult
toRollResult description generator =
    Random.map (oneDieResult description) generator



-- Local Functions


successes : (RollResult -> Bool) -> RollResult -> RollResult
successes test rollResult =
    let
        num =
            case rollResult.children of
                Empty ->
                    if test rollResult then
                        1

                    else
                        0

                RollResults rolls ->
                    count test rolls
    in
    { rollResult | value = num }


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


constant : String -> Int -> Random.Generator RollResult
constant description val =
    Random.constant val
        |> Random.map (oneDieResult description)


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

        DicePool description generator ->
            dCompound description generator

        Constant description val ->
            constant description val

        CustomDie description sides ->
            dCustom description sides

        WeightedDie description sides ->
            dWeighted description sides


dWeighted : String -> List ( Float, Int ) -> Random.Generator RollResult
dWeighted description sides =
    case sides of
        [] ->
            constant description 0

        x :: xs ->
            Random.weighted x xs
                |> toRollResult description


dCustom : String -> List Int -> Random.Generator RollResult
dCustom description sides =
    case sides of
        [] ->
            constant description 0

        x :: xs ->
            Random.uniform x xs
                |> toRollResult description


dCompound : String -> Random.Generator RollResult -> Random.Generator RollResult
dCompound desc generator =
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

        DicePool description _ ->
            description

        Constant description _ ->
            description

        CustomDie description _ ->
            description

        WeightedDie description _ ->
            description


reroll : Random.Generator RollResult -> (RollResult -> Bool) -> RollResult -> Random.Generator (List RollResult)
reroll generator test rollResult =
    if test rollResult then
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


explode : Random.Generator RollResult -> (RollResult -> Bool) -> RollResult -> Random.Generator (List RollResult)
explode generator test rollResult =
    explodeLimited generator test 1 rollResult


maxRecursion =
    100


explodeLimited : Random.Generator RollResult -> (RollResult -> Bool) -> Int -> RollResult -> Random.Generator (List RollResult)
explodeLimited generator test recursionCount rollResult =
    if (recursionCount < maxRecursion) && test rollResult then
        Random.constant rollResult
            |> Random.list 1
            |> Random.map2 (++) (generator |> Random.andThen (explodeLimited generator test (recursionCount + 1)))

    else
        Random.constant rollResult
            |> Random.list 1


dropLowestRoll : RollResult -> RollResult
dropLowestRoll result =
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
