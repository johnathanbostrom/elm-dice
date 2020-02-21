module SimpleRoller exposing (main)
import Browser
import Dice exposing (..)
import Random exposing(generate)
import Html exposing (Html, div, button, text, span)
import Html.Attributes exposing (style)
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
    ( { rolls = [ ]}
    , Cmd.none
    )



-- MODEL


type alias Model =
    { rolls : List RollResult
    }



-- UPDATE


type Msg
    = RollDice Int Dice
    | RollResult (List RollResult)


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


rollDice : Int -> Dice -> Cmd Msg
rollDice num dieType =
    roll num dieType
    |> Random.generate RollResult

statGen : Dice
statGen =
    roll 4 D6
    -- |> andThen DropLowest
    -- |> andThen CombineResults
    |> DCustom

plusRoll : Dice
plusRoll =
    roll 2 (DX 4)
    -- |> plus (roll 2 (DX 6) |> andThen DropLowest)
    |> DCustom

rResultToString : (List RollResult) -> String
rResultToString results =
    List.map (\r ->  rollToString r) results
    |> String.join "~"

rollToString : RollResult -> String
rollToString result =
    case result of
        OneDie d ->
            d.dieType ++ ":  " ++ (String.fromInt d.value)
        MultipleDice m ->
            (String.fromInt m.value) ++ ":  Rolls:  " ++ (rResultToString m.rolls)

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
        [ text <| String.join "  " <| List.map String.fromInt <| justResults model.rolls
        , diceButtons
        , renderExpandedResults model.rolls
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
        , diceButton (DCustom (roll 2 (DCustom <| roll 2 (DCustom <| roll 2 statGen)))) "3D6"
        , diceButton statGen "statGen"
        , diceButton plusRoll "2"
        ]

diceButton : Dice -> String -> Html Msg
diceButton dieType dieName =
    button [ onClick <| RollDice 6 dieType ] [text dieName]


renderExpandedResults : List RollResult -> Html Msg
renderExpandedResults rolls =
    div [] (List.map renderExpandedResult rolls)

renderExpandedResult : RollResult -> Html Msg
renderExpandedResult roll =
    case roll of
        OneDie d ->
            span [style "margin-right" ".5em"] [text <| d.dieType ++ ":  " ++ String.fromInt d.value]
        MultipleDice m ->
            div [style "margin-left" "2em"] ([text <| "Value:  " ++ String.fromInt m.value ++ "  Rolls:  "] ++ (List.map renderExpandedResult m.rolls))

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none