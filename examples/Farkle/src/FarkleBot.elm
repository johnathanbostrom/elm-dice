module FarkleBot exposing (automatedFarkleTurn, BotType)
import Random
import List.Extra exposing (gatherEquals, subsequences, minimumBy, maximumBy)
import Helpers exposing (allMaxBy)
import Random.Extra exposing (andThen2)
import Dice exposing (RollResult,  ChildrenRolls(..), mapValue)
import Farkle exposing (farkleScore, farkleRoll)

type BotType = Dumb 

automatedFarkleTurn : Random.Generator RollResult
automatedFarkleTurn =
    let
        firstRoll = farkleRoll 6
        farkleRolls = Random.constant <| RollResult "farkle rolls" 0 Empty
    in
        andThen2 rollFarkleTurn farkleRolls firstRoll


rescoreFarkleRoll : FarkleStep -> RollResult -> RollResult
rescoreFarkleRoll diceKept roll =
    let
        newDescription =
            "Rolled " ++ roll.description ++ " ... " ++
            if diceKept.score < 1 then
                "Farkle!"
            else
                "Kept " ++ " [" ++ (String.join ", " <| List.map String.fromInt diceKept.dice) ++ "]"
    in
        {roll | value = diceKept.score, description = newDescription}

rollFarkleTurn : RollResult -> RollResult -> Random.Generator RollResult
rollFarkleTurn farkleRolls latestRoll =
    let
        keep = maxThisRoll farkleRolls.value latestRoll
        newScore = farkleRolls.value + keep.score
        rescoredRoll = rescoreFarkleRoll keep latestRoll  
        combinedRolls = combineFarkleRolls farkleRolls rescoredRoll
        combinedRollGenerator = Random.constant combinedRolls 
    in
        if keep.score == 0 then
            combinedRollGenerator
                |> mapValue (\_ -> 0)
                |> Random.andThen (\r -> Random.constant {r | description = "Farkle!"})
        else if shouldReroll newScore keep.diceLeft then
            farkleRoll keep.diceLeft
            |> mapValue (\r -> (maxThisRoll newScore r).score)
            |> andThen2 rollFarkleTurn combinedRollGenerator
        else
            combinedRollGenerator    

combineFarkleRolls : RollResult -> RollResult -> RollResult
combineFarkleRolls farkleRolls latestRoll = 
    let
        newChildren = 
            case farkleRolls.children of
                Empty ->
                    [latestRoll]  
                RollResults rolls ->
                    latestRoll :: rolls
        newValue = farkleRolls.value + latestRoll.value
    in
        {farkleRolls | value = newValue, children = RollResults newChildren}


shouldReroll : Int -> Int -> Bool
shouldReroll score numDice =
    let
        expected = toFloat <| expectedValue numDice
        loss = toFloat score * (chanceToFarkle numDice)
    in
        not (loss > expected)



maxThisRoll : Int -> RollResult -> FarkleStep
maxThisRoll currentScore roll =
    validFarkleSteps roll
        |> keepMaxWithLeastDice (scoreFarkleStep currentScore)
        |> Maybe.withDefault {dice = [], diceLeft = 0, score = 0}
                

scoreFarkleStep : Int -> FarkleStep -> Int
scoreFarkleStep currentScore farkleStep =
    if shouldReroll (currentScore + farkleStep.score) farkleStep.diceLeft then
        farkleStep.score + expectedValue farkleStep.diceLeft
    else
        farkleStep.score


validFarkleSteps : RollResult -> List FarkleStep
validFarkleSteps rollResult =
    case rollResult.children of
        Empty ->
            []
        RollResults rolls ->
            subsequences rolls
                |> List.filter (\s -> List.length s > 0)
                |> List.map (\rs -> toFarkleStep rolls rs)
                |> List.filter (\s -> s.score > 0)

keepMaxWithLeastDice : (FarkleStep -> comparable) -> List FarkleStep -> Maybe FarkleStep
keepMaxWithLeastDice maxCalc possibleDice =
    allMaxBy maxCalc possibleDice
    |> maximumBy .diceLeft


type alias FarkleStep = { score : Int
                        , dice : List Int
                        , diceLeft : Int
                        }


toFarkleStep : List RollResult -> List RollResult -> FarkleStep
toFarkleStep allRolls rollsKept =
    List.length allRolls - List.length rollsKept
        |> FarkleStep (farkleScore <| List.map .value rollsKept) (List.map .value rollsKept)


expectedValue : Int -> Int
expectedValue numDice =
    case numDice of
        6 -> 300
        5 -> 200
        4 -> 150
        3 -> 90 
        2 -> 50
        1 -> 25
        _ -> 0


chanceToFarkle : Int -> Float
chanceToFarkle numDice =
    case numDice of
        6 -> 0.01
        5 -> 0.07
        4 -> 0.20
        3 -> 0.30
        2 -> 0.40
        1 -> 0.70
        _ -> 1