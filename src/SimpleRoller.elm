module SimpleRoller exposing (main)
import Browser
import Dice exposing (..)
import Random exposing(generate)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

-- APP


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { rolls = [ ], eRolls = ""
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { rolls : List Int
    , eRolls : String
    }



-- UPDATE


type Msg
    = RollDice Int Dice
    | RollResult (List Int)
    | RollResultExpanded RollResult
    | RollExpanded


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice num dice ->
            ( model
            , rollDice num dice
            )
        RollResult newRolls ->
            ( {model | rolls = newRolls }
            , Cmd.none
            )
        RollResultExpanded newRolls ->
            ({model | eRolls = rResultToString newRolls}, Cmd.none)
        RollExpanded ->
            (model, expandedRoll)


rollDice : Int -> Dice -> Cmd Msg
rollDice num dieType =
    roll num dieType
    |> Random.generate RollResult

statGen : Dice
statGen =
    roll 4 D6
    |> andThen DropLowest
    |> andThen CombineResults
    |> DCustom

plusRoll : Dice
plusRoll =
    roll 2 (DX 4)
    |> plus (roll 2 (DX 6) |> andThen DropLowest)
    |> DCustom

expandedRoll : Cmd Msg
expandedRoll =
    rollExpanded 1 statGen
    |> Random.generate RollResultExpanded

rResultToString : RollResult -> String
rResultToString result =
    String.fromInt result.value ++ " Rolls: " ++ dResultToString result.rolls

dResultToString : List DieResult -> String
dResultToString result =
    List.map (\r -> r.dieType ++ ": " ++ String.fromInt r.value) result
    |> String.join " "

-- VIEW


view : Model -> Html Msg
view model =
    div [] [renderDice model]


renderDice : Model-> Html Msg
renderDice model =
    div
        [ ]
        [ text <| String.join "  " <| List.map String.fromInt model.rolls
        , diceButtons
        , text <| model.eRolls
        ]


diceButtons : Html Msg
diceButtons = 
    div [] 
        [ diceButton D4 "D4" 
        , diceButton D6 "D6"
        , diceButton D8 "D8"
        , diceButton D10 "D10"
        , diceButton D12 "D12"
        , diceButton D20 "D20"
        , diceButton D100 "D100"
        , diceButton (DCustom (roll 3 D6)) "3D6"
        , diceButton statGen "statGen"
        , diceButton plusRoll "2"
        , expandedButton
        ]


diceButton : Dice -> String -> Html Msg
diceButton dieType dieName =
    button [ onClick <| RollDice 6 dieType ] [text dieName]

expandedButton : Html Msg
expandedButton =
    button [ onClick <| RollExpanded] [text "EXPAND"]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none