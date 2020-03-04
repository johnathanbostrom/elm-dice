module Farkle exposing (main, farkleScore, dieGroupScore, maxThisRoll, toFarkleStep, validFarkleSteps, scoreFarkleStep)
import Dice exposing (..)
import Browser
import Random
import List.Extra exposing (gatherEquals, subsequences, minimumBy, maximumBy)
import Html exposing (div, Html, text, span)
import Html.Attributes exposing (style)
import Helpers exposing (allMaxBy)
import Random.Extra exposing (andThen2)

type alias Model =
    { computerRolls : Maybe RollResult
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initModel : Model
initModel =
    { computerRolls = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            initModel
    in
    ( initModel
    , Cmd.batch [computerTurn]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ComputerRoll
    | GotComputerRoll RollResult

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComputerRoll -> 
            (model, computerTurn)
        GotComputerRoll roll ->
            ({model | computerRolls = Just roll}, Cmd.none)


farkleRoll : Int -> Random.Generator RollResult
farkleRoll numDice =
    roll numDice D6
    

computerTurn : Cmd Msg
computerTurn =
    automatedFarkleTurn
    |> Random.generate GotComputerRoll

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


farkled : List FarkleStep -> Bool
farkled possibleKeep =
    List.length possibleKeep < 1


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



view : Model -> Html Msg
view model = 
    case model.computerRolls of
        Nothing -> 
            div[][]
        Just rollResults ->
            renderRoll rollResults



renderExpandedResults : List RollResult -> Html Msg
renderExpandedResults rolls =
    div [] (List.map renderRoll rolls)


renderRoll : RollResult -> Html Msg
renderRoll roll =
    let
        valueString =
            roll.description ++ ":  " ++ String.fromInt roll.value
    in
    case roll.children of
        Empty ->
            span [ style "margin-right" ".5em" ] [ text <| valueString ]

        RollResults rolls ->
            let
                children =
                    div [ style "margin-left" "2em" ] (List.map renderRoll rolls)
            in
            div [] [ text <| valueString, children ]