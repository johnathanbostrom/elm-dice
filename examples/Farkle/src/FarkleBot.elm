module FarkleBot exposing (automatedFarkleTurn, BotType, justRolls, FarkleTurnResult, FarkleRollResult)
import Random
import List.Extra exposing (gatherEquals, subsequences, minimumBy, maximumBy)
import Helpers exposing (allMaxBy)
import Random.Extra exposing (andThen2)
import Dice exposing (RollResult, ChildrenRolls(..), mapValue)
import Farkle exposing (farkleScore, farkleRoll)

type BotType = Dumb 

justRolls : RollResult -> List RollResult
justRolls rollResult =
    case rollResult.children of
        Empty ->
            []  
        RollResults rolls ->
            rolls
        

automatedFarkleTurn : Random.Generator FarkleTurnResult
automatedFarkleTurn =
    let
        firstRoll = farkleRoll 6
        farkleRolls = Random.constant <| FarkleTurnResult 0 []
    in
        andThen2 rollFarkleTurn2 farkleRolls firstRoll


rollFarkleTurn2 : FarkleTurnResult -> RollResult -> Random.Generator FarkleTurnResult
rollFarkleTurn2 farkleTurn latestRoll =
    let 
        keep = maxThisRoll farkleTurn.score latestRoll
        newTurnValue = 
            if keep.score == 0 then 
                0 
            else 
                farkleTurn.score + keep.score
        generator = Random.constant { farkleTurn | rolls = keep :: farkleTurn.rolls, score = newTurnValue } 
    in
        if shouldReroll newTurnValue keep.diceLeft then
            farkleRoll keep.diceLeft
                |> andThen2 rollFarkleTurn2 generator
        else
            generator


    -- let
    --     keep = maxThisRoll farkleRolls.value latestRoll
    --     newScore = farkleRolls.value + keep.score
    --     rescoredRoll = rescoreFarkleRoll keep latestRoll  
    --     combinedRolls = combineFarkleRolls2 farkleRolls rescoredRoll
    --     fRoll = FarkleRollResult newScore rescoredRoll keep.rolls
    --     combinedRollGenerator = Random.constant combinedRolls 
    -- in
    --     if keep.score == 0 then
    --         combinedRollGenerator
    --             |> mapValue (\_ -> 0)
    --             |> Random.andThen (\r -> Random.constant {r | description = "Farkle!"})
    --     else if shouldReroll newScore keep.diceLeft then
    --         farkleRoll keep.diceLeft
    --         |> mapValue (\r -> (maxThisRoll newScore r).score)
    --         |> andThen2 rollFarkleTurn2 combinedRollGenerator
    --     else
    --         combinedRollGenerator    





rescoreFarkleRoll : FarkleRollResult -> RollResult -> RollResult
rescoreFarkleRoll diceKept roll =
    let
        newDescription =
            "Rolled " ++ roll.description ++ " ... " ++
            if diceKept.score < 1 then
                "Farkle!"
            else
                "Kept " ++ " [" ++ (String.join ", " <| List.map (\r -> String.fromInt r.value) diceKept.kept) ++ "]"
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



maxThisRoll : Int -> RollResult -> FarkleRollResult
maxThisRoll currentScore roll =
    validFarkleRollResults roll
        |> keepMaxWithLeastDice (scoreFarkleRollResult currentScore)
        |> Maybe.withDefault {roll = roll, kept = [], diceLeft = 0, score = 0}
                

scoreFarkleRollResult : Int -> FarkleRollResult -> Int
scoreFarkleRollResult currentScore farkleRollResult =
    if shouldReroll (currentScore + farkleRollResult.score) farkleRollResult.diceLeft then
        farkleRollResult.score + expectedValue farkleRollResult.diceLeft
    else
        farkleRollResult.score


validFarkleRollResults : RollResult -> List FarkleRollResult
validFarkleRollResults rollResult =
    case rollResult.children of
        Empty ->
            []
        RollResults rolls ->
            subsequences rolls
                |> List.filter (\s -> List.length s > 0)
                |> List.map (\rs -> toFarkleRollResult rollResult rs (List.length rolls - List.length rs))
                |> List.filter (\s -> s.score > 0)

keepMaxWithLeastDice : (FarkleRollResult -> comparable) -> List FarkleRollResult -> Maybe FarkleRollResult
keepMaxWithLeastDice maxCalc possibleDice =
    allMaxBy maxCalc possibleDice
    |> maximumBy .diceLeft



type alias FarkleRollResult =
    { score : Int
    , roll : RollResult
    , kept : List (RollResult)
    , diceLeft : Int
    }

type alias FarkleTurnResult =
    { score : Int
    , rolls : List FarkleRollResult
    }


toFarkleRollResult : RollResult -> List RollResult -> Int -> FarkleRollResult
toFarkleRollResult allRolls rollsKept numLeft =
    FarkleRollResult (farkleScore <| List.map .value rollsKept) allRolls rollsKept numLeft


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