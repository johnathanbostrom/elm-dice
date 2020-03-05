module Main exposing (main)
import Browser
import Html exposing (div, Html, text, span, button)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Dice exposing (RollResult, ChildrenRolls(..))
import Random
import FarkleBot exposing (automatedFarkleTurn)
import Farkle exposing (farkleRoll, farkleScore, farkled)
import List.Extra exposing (getAt, updateAt)

type alias Model =
    { computerRolls : Maybe RollResult
    , playerRolls : Maybe (List PlayerRollDie)
    , playerA : Player
    , playerB : Player
    , farkled : Bool
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


initModel : Model
initModel =
    { computerRolls = Nothing
    , playerRolls = Nothing
    , playerA = initPlayer "Player"
    , playerB = initPlayer "Computer"
    , farkled = False
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



type Msg
    = ComputerRoll
    | GotComputerRoll RollResult
    | PlayerRoll Int Int
    | GotPlayerRoll RollResult
    | PlayerStop Int
    | PickDieToKeep Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComputerRoll -> 
            (model, computerTurn)
        GotComputerRoll roll ->
            let
                oldPlayer = model.playerB
                updatedPlayer = 
                    {oldPlayer | turnScore = 0, totalScore = oldPlayer.totalScore + oldPlayer.turnScore + roll.value}
            in
            ({model | computerRolls = Just roll, playerB = updatedPlayer}, Cmd.none)
        PlayerRoll score numDice->
            let
                oldPlayer = model.playerA
                updatedPlayer = 
                    {oldPlayer | turnScore = oldPlayer.turnScore + score}
            in
                ({model | playerA = updatedPlayer}, playerRoll numDice)
        GotPlayerRoll roll ->
            let
                oldPlayer = model.playerA
                (updatedPlayer, rollFarkled) =   
                    if farkled roll then
                        ({oldPlayer | turnScore = 0 }, True)
                    else
                        (oldPlayer, False)
            in
                ({model | playerRolls = Just (rollResultToPlayerRollDie roll), playerA = updatedPlayer, farkled = rollFarkled}, Cmd.none)
        PlayerStop score ->
            let
                oldPlayer = model.playerA
                updatedPlayer = 
                    {oldPlayer | turnScore = 0, totalScore = oldPlayer.totalScore + oldPlayer.turnScore + score}
            in
                ({model | playerA = updatedPlayer, playerRolls = Nothing }, computerTurn)
        PickDieToKeep index ->
            let
                updatedDice = 
                    case model.playerRolls of
                        Just rolls ->
                            rolls
                                |> updateAt index 
                                    (\r -> 
                                        let
                                            wasSelected = r.selected
                                        in
                                            {r| selected = not wasSelected}
                                    )
                                |> Just
                        Nothing ->
                            Nothing
            in
                ({model | playerRolls = updatedDice}, Cmd.none)


computerTurn : Cmd Msg
computerTurn =
    automatedFarkleTurn
    |> Random.generate GotComputerRoll

playerRoll : Int -> Cmd Msg
playerRoll numDice = 
    farkleRoll numDice
        |> Random.generate GotPlayerRoll

rollResultToPlayerRollDie : RollResult -> List PlayerRollDie
rollResultToPlayerRollDie rollResult =
    case rollResult.children of
        Empty ->
            []
        RollResults rolls ->
            List.map (\r -> PlayerRollDie r.value False) rolls

type alias PlayerRollDie =
    { value : Int
    , selected : Bool
    }

type alias Player =
    { turnScore : Int
    , totalScore : Int
    , name : String
    }

initPlayer : String -> Player
initPlayer name = 
    Player 0 0 name

selectedScore : List PlayerRollDie -> Int
selectedScore selectedRolls =
    farkleScore <| List.map .value <| List.filter .selected selectedRolls

        

view : Model -> Html Msg
view model =
    div [ style "display" "grid"
        , style "grid-template-columns" "auto 60px auto 60px auto"
        , style "grid-template-rows" "150px 50px 10px 50px auto 10px"
        , style "height" "100vh"
        , style "grid-template-areas" templateAreas
        ]
        [ playersContainer model
        , selectedDiceContainer model
        , currentDiceContainer model
        , buttonContainer model
        ]


templateAreas : String 
templateAreas =
    String.join " "    
        [ "\". p p p .\"" 
        , "\". o d t .\""
        , "\". o . t .\""
        , "\". o b t .\""    
        , "\". o . t .\""
        , "\". o . t .\""
        ]


buttonContainer : Model -> Html Msg
buttonContainer model =
    div [ class "buttonContainer"
        , style "grid-area" "b" 
        , style "justify-self" "center" 
        ]  <| buttons model


playersContainer : Model -> Html Msg
playersContainer model =
    div [ class "playersContainer"
        , style "grid-area" "p"       
        ] 
        [ playerView model.playerA
        , playerSpacer
        , playerView model.playerB
        ]


playerView : Player -> Html Msg
playerView player =
    div [ style "display" "inline-block"
        , style "width" "30%"
        , class "playerView"
        ] 
        [ div [] [ text player.name ]
        , div [] [ text <| "SCORE: " ++ String.fromInt player.totalScore ]
        , div [] [ text <| "Current Round: " ++ String.fromInt player.turnScore ]
        ]

playerSpacer : Html Msg
playerSpacer =
    div [ style "width" "30%"
        , style "display" "inline-block"
        ]
        []

buttons : Model -> List (Html Msg)
buttons model =
    case (model.playerRolls, model.farkled) of
        (Nothing, _) -> 
            [btn "Roll" <|  PlayerRoll 0 6]
        (Just rolls, False) ->
            let
                selectedRolls = List.filter .selected rolls
                score = selectedScore rolls
                unkept = (List.length rolls) - (List.length selectedRolls)
            in
                [ btn "Roll Again" <| PlayerRoll score unkept
                , btn "Stay" <| PlayerStop score
                ]
        (Just rolls, True) ->
            [btn "End Turn" <| PlayerStop 0]

btn : String -> Msg ->  Html Msg
btn name click =
    button [ onClick click ][ text name ] 


selectedDiceContainer : Model ->  Html Msg
selectedDiceContainer model =
    div [ style "height" "100%"
        , style "width" "100%"
        , style "background-color" "lightgray"
        , style "grid-area" "o"
        ]
        []

currentDiceContainer : Model -> Html Msg
currentDiceContainer model =
    let
        attrs = 
            [ class "currentDiceContainer"] ++ currentDiceContainerStyles
        rolls =
            case model.playerRolls of
                Nothing ->
                    []
                Just r ->
                    r           
    in
        div attrs <| diePickers rolls
              


diceScorer : List PlayerRollDie -> Html Msg
diceScorer rolls =
    span [] [text <| String.fromInt <| selectedScore rolls]


diePickers : List PlayerRollDie ->  List (Html Msg)
diePickers rolls =
    let
        emptyContainers = List.repeat (6 - List.length rolls) emptyDiePicker
    in
        (List.indexedMap diePicker rolls) ++ emptyContainers

diePickerWrapper : Html Msg -> Html Msg
diePickerWrapper dPicker =
    div ([ class "diePickerWrapper" ] ++ diePickerWrapperStyles)
         [dPicker]

diePicker : Int -> PlayerRollDie -> Html Msg
diePicker index die =
    let
        background = 
            if die.selected then
                "lightgoldenrodyellow"
            else
                "lightgrey"
        attrs = [ style "background-color" background 
                , onClick <| PickDieToKeep index
                ] ++ diePickerStyles
    in
        span attrs [ text <| String.fromInt die.value ]

emptyDiePicker : Html Msg
emptyDiePicker =
    span ([style "background-color" "lightgrey"] ++ diePickerStyles) []

viewComputerRolls : Model -> Html Msg
viewComputerRolls model =
    case model.computerRolls of
        Nothing -> 
            div [][]
        Just rolls ->
            renderRoll rolls

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


currentDiceContainerStyles : List (Html.Attribute Msg)
currentDiceContainerStyles =
    [ style "grid-area" "d"
    , style "display" "grid"
    , style "grid-auto-colums" "40px"
    , style "grid-auto-flow" "column"
    , style "justify-self" "center"
    , style "grid-column-gap" "20px"
    ]


diePickerStyles : List (Html.Attribute Msg)
diePickerStyles =
    [ style "display" "inline-block"
    , style "width" "40px"
    , style "height" "40px"
    ]

diePickerWrapperStyles : List (Html.Attribute Msg)
diePickerWrapperStyles =
    [ style "width" "15%"
    , style "display" "inline-block"
    ]