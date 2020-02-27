module SimpleRoller exposing (main)

import Browser
import Dice exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (generate)



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
    ( { rolls = [] }
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
            ( { model | rolls = newRolls }
            , Cmd.none
            )


rollDice : Int -> Dice -> Cmd Msg
rollDice num roller =
    roll 1 D10 
        |> explodeIf (\x -> x > 0) 
        |> countSuccessesIf (\x -> True)
        |> Random.list 1 
        |> Random.generate RollResult
    -- roll 1 roller
    --     |> Random.list num 
    --     |> Random.generate RollResult


statGen : Dice
statGen =
    roll 4 D6
        |> dropLowest
        |> CustomDie "4D6 Drop Lowest"


plusRoll : Dice
plusRoll =
    roll 3 D6
        |> dropLowest
        |> plus (roll 2 D4)
        |> CustomDie "3D6 Drop Lowest plus 2D4"


succeedOnEightRoll : Dice
succeedOnEightRoll =
    roll 1 explodingD10
        |> CustomDie "Successes"


explodingD10 : Dice
explodingD10 =
    roll 1 D10 
        |> explodeIf (\x -> x > 9)
        -- |> countSuccessesIf (\x -> x > 7)
        |> CustomDie "exploding Dice"


aT : Dice
aT =
    roll 3 D6
        |> andThen (\x -> Random.constant { x | value = -100 })
        |> CustomDie "test"


rerollDice : Dice
rerollDice =
    roll 1 statGen
        |> rerollIf (\x -> x < 16) High
        |> CustomDie "reroll < 3"


rResultToString : List RollResult -> String
rResultToString results =
    List.map (\r -> rollToString r) results
        |> String.join "~"


rollToString : RollResult -> String
rollToString result =
    case result.children of
        Empty ->
            result.description ++ ":  " ++ String.fromInt result.value

        RollResults rolls ->
            String.fromInt result.value ++ ":  Rolls:  " ++ rResultToString rolls


dResultToString : List RollResult -> String
dResultToString result =
    List.map (\r -> r.description ++ ": " ++ String.fromInt r.value) result
        |> String.join " "



-- VIEW
view : Model -> Html Msg
view model =
    div [] [ renderDice model ]


renderDice : Model -> Html Msg
renderDice model = 
    case model.rolls of
        [] ->
            div [] [ text "Click A Button To Roll Dice", diceButtons ]

        _ ->
            div
                []
                [ text <| String.join "  " <| List.map (\r -> String.fromInt r.value) model.rolls
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
        , diceButton (roll 3 D6 |> CustomDie "3D6") "3D6"
        , diceButton statGen "statGen"
        , diceButton plusRoll "2"
        , diceButton succeedOnEightRoll "Exploding Dice Succeed on 8"
        , diceButton aT "Test"
        ]


diceButton : Dice -> String -> Html Msg
diceButton dieType dieName =
    button [ onClick <| RollDice 6 dieType ] [ text dieName ]



renderExpandedResults : List RollResult -> Html Msg
renderExpandedResults rolls =
    div [] (List.map renderRoll rolls)


renderExpandedResult : RollResult -> Html Msg
renderExpandedResult roll =
    let
        valueString =
            roll.description ++ ":  " ++ String.fromInt roll.value
    in
    case roll.children of
        Empty ->
            span [ style "margin-right" ".5em" ] [ text <| valueString ]

        RollResults rolls ->
            div [ style "margin-left" "2em" ] ([ text <| valueString ++ "  Rolls:  " ] ++ List.map renderExpandedResult rolls)


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
