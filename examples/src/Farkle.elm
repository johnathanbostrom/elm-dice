module Farkle exposing (main, farkleScore, dieGroupScore)
import Dice exposing (..)
import Browser
import Random
import List.Extra exposing (gatherEquals)
import Html exposing (div, Html)

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
    , Cmd.batch [ ]
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
            (model, Cmd.none)
        GotComputerRoll roll ->
            ({model | computerRolls = Just roll}, Cmd.none)


farkleRoll : Int -> Random.Generator RollResult
farkleRoll numDice =
    roll numDice D6

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
    gatherEquals vals
    |> List.any (\x -> List.length (Tuple.second x) /= 1 ) 
    |> not



view : Model -> Html Msg
view model = div [][]