module SimpleRoller exposing (main)
import Dice exposing (..)
import Random exposing(generate)
import Html exposing (div, button, text)
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
    ( { nums = [ ]
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { rolls : List Int
    }



-- UPDATE


type Msg
    = RollDice Int Dice
    | RollResult List Int


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

-- VIEW


view : Model -> Html Msg
view model =
    div [] [renderDice model.rolls]


renderDice : List Int -> Html Msg
renderDice rolls =
    div
        [ ]
        [ text <| String.join "  " <| List.map String.fromInt rolls]


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
        ]


diceButton : Dice -> String -> Html Msg
diceButton dieType dieName =
    button [ onClick roll 1 dieType ] [text dieName]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none