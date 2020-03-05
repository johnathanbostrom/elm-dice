module Farkle exposing (farkleScore, farkleRoll, farkled)
import Dice exposing (..)
import Random
import List.Extra exposing (gatherEquals)


farkleRoll : Int -> Random.Generator RollResult
farkleRoll numDice =
    roll numDice D6



farkled : RollResult -> Bool
farkled roll =
    case roll.children of
        Empty ->
            True
        RollResults rolls ->
            List.map .value rolls
            |> farkleScore
            |> (==) 0


farkleScore : List Int -> Int
farkleScore vals =
    if isStraight vals then
        1000
    else if isThreePair vals then
        500
    else
        gatherEquals vals
            |> List.map dieGroupScore
            |> List.sum


dieGroupScore : (Int, List Int) -> Int
dieGroupScore group =
    let
        num = (List.length <| Tuple.second group) + 1
        val = Tuple.first group
        tripleScores = (tripleScore val) * (num//3)
        singleScores = singleScore val  * (modBy 3 num)
    in 
        tripleScores + singleScores
        


singleScore : Int -> Int
singleScore val =
    case val of
        1 -> 100
        5 -> 50
        _ -> 0


tripleScore : Int -> Int
tripleScore val =
    case val of 
        1 -> 1000
        2 -> 200
        3 -> 300
        4 -> 400
        5 -> 500
        6 -> 600
        _ -> 0

isStraight : List Int -> Bool
isStraight vals = 
    case List.sort vals of
        [1,2,3,4,5,6] -> True
        _ -> False

isThreePair : List Int -> Bool
isThreePair vals =
    if List.length vals < 6 then
        False
    else
        gatherEquals vals
        |> List.any (\x -> List.length (Tuple.second x) /= 1 ) 
        |> not
