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
    , currentRolls : Maybe (List PlayerRollDie)
    , currentPlayers : Players
    , farkled : Bool
    , turnScore : Int
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
    , currentRolls = Nothing
    , currentPlayers = Players (initPlayer "Player" Human) (initPlayer "Computer" Computer) FirstPlayer
    , farkled = False
    , turnScore = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            initModel
    in
    ( initModel
    , Cmd.batch [playerRoll 6]
    )



type Msg
    = GotComputerRoll RollResult
    | GotPlayerRoll RollResult
    | PlayerStop Int
    | PickDieToKeep Int
    | RollDice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotComputerRoll roll ->
            endTurn {model | turnScore = roll.value}
        GotPlayerRoll roll ->
            let
                rollFarkled = farkled roll
                updatedTurnScore =
                    if rollFarkled then
                        0
                    else
                        model.turnScore
            in
                ({model | currentRolls = Just (rollResultToPlayerRollDie roll), turnScore = updatedTurnScore, farkled = rollFarkled}, Cmd.none)
        PlayerStop score ->
            endTurn {model | turnScore = model.turnScore + score}
        PickDieToKeep index ->
            let
                updatedDice = 
                    case model.currentRolls of
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
                ({model | currentRolls = updatedDice}, Cmd.none)
        RollDice ->
            case (currentPlayer model.currentPlayers).playerType of
                Human ->
                    case model.currentRolls of
                        Just rolls ->                        
                            let
                                selectedRolls = List.filter .selected rolls
                                numDice = (List.length rolls) - (List.length selectedRolls)
                                score = farkleScore <| List.map .value selectedRolls
                            in
                                ({model | turnScore = model.turnScore + score}, playerRoll numDice)
                        Nothing ->
                            (model, playerRoll 6)
                Computer ->
                    (model, computerTurn)


endTurn : Model -> (Model, Cmd Msg)
endTurn model =
    let
        updatedPlayers =
            updateCurrentPlayer (\p -> {p | totalScore = p.totalScore + model.turnScore}) model.currentPlayers
            |> nextTurn
        cmd =
            case (currentPlayer updatedPlayers).playerType of
                Human ->
                    playerRoll 6
                Computer ->
                    computerTurn
    in
        ({model | currentPlayers = updatedPlayers, turnScore = 0}, cmd)



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
    { totalScore : Int
    , name : String
    , playerType : PlayerType
    }

type Players = Players Player Player CurrentPlayer

type CurrentPlayer = FirstPlayer
    | SecondPlayer

type PlayerType = Human
    | Computer

updateCurrentPlayer  : (Player -> Player) -> Players -> Players
updateCurrentPlayer fn players =
    case players of
        Players a b FirstPlayer ->
            Players (fn a) b FirstPlayer
        Players a b SecondPlayer ->
            Players a (fn b) SecondPlayer
    
currentPlayer : Players -> Player
currentPlayer players =
    case players of 
        Players a _ FirstPlayer ->
            a
        Players _ b SecondPlayer ->
            b

nextTurn : Players -> Players
nextTurn players =
    case players of
        Players a b FirstPlayer ->
            Players a b SecondPlayer
        Players a b SecondPlayer ->
            Players a b FirstPlayer

playerOne : Players -> Player
playerOne players =
    case players of
        Players a _ _ ->
            a

playerTwo : Players -> Player
playerTwo players =
    case players of
        Players _ b _ ->
            b

isPlayersTurn : CurrentPlayer -> Players -> Bool
isPlayersTurn turn players =
    case players of
        Players _ _ t ->
            t == turn


initPlayer : String -> PlayerType -> Player
initPlayer name playerType = 
    Player 0 name playerType
        

view : Model -> Html Msg
view model =
    div [ style "display" "grid"
        , style "grid-template-columns" "auto 70px 520px 70px auto"
        , style "grid-template-rows" "150px 60px 10px 60px 350px auto"
        , style "height" "100vh"
        , style "grid-template-areas" templateAreas
        ]
        [ playersContainer model
        , selectedDiceContainer model FirstPlayer
        , currentDiceContainer model
        , selectedDiceContainer model SecondPlayer
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
        , "\". . . . .\""
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
        [ playerView <| playerOne model.currentPlayers
        , playerSpacer
        , playerView <| playerTwo model.currentPlayers
        ]


playerView : Player -> Html Msg
playerView player =
    div [ style "display" "inline-block"
        , style "width" "30%"
        , class "playerView"
        ] 
        [ div [] [ text player.name ]
        , div [] [ text <| "SCORE: " ++ String.fromInt player.totalScore ]
        ]

playerSpacer : Html Msg
playerSpacer =
    div [ style "width" "30%"
        , style "display" "inline-block"
        ]
        []

buttons : Model -> List (Html Msg)
buttons model =
    case (model.currentRolls, model.farkled) of
        (Nothing, _) -> 
            [btn "Roll" <|  RollDice]
        (Just rolls, False) ->
            let
                selectedRolls = List.filter .selected rolls
                score = farkleScore <| List.map .value <| List.filter .selected rolls
                unkept = (List.length rolls) - (List.length selectedRolls)
            in
                [ btn "Roll Again" <| RollDice
                , btn "Stay" <| PlayerStop score
                ]
        (Just rolls, True) ->
            [btn "End Turn" <| PlayerStop 0]

btn : String -> Msg ->  Html Msg
btn name click =
    button [ onClick click ][ text name ] 


selectedDiceContainer : Model -> CurrentPlayer -> Html Msg
selectedDiceContainer model owningPlayer =
    let
        rolls = 
            case (isPlayersTurn owningPlayer model.currentPlayers, model.currentRolls) of
                (True, Just rs) ->
                    rs
                _ ->
                    []
        gridArea = 
            case owningPlayer of
                FirstPlayer ->
                    "o"
                SecondPlayer ->
                    "t"
    in
        div [ style "height" "100%"
            , style "width" "100%"
            , style "background-color" "lightgray"
            , style "grid-area" gridArea
            , style "display" "grid"
            , style "grid-template-rows" "50px auto"
            , style "grid-auto-flow" "row"
            ]
            [ diceScorer rolls
            , selectedDiceView rolls 
            ]

selectedDiceView : List PlayerRollDie -> Html Msg
selectedDiceView rolls =
    div [ style "display" "grid"
        , style "grid-auto-rows" dieSize
        , style "grid-auto-flow" "row"
        , style "grid-row-gap" "5px"
        , style "grid-row" "2 / 2"
        , style "border" "1px solid darkgrey"
        , style "justify-content" "center"
        , style "padding-top" "5px"
        ]
        (diePickersWithoutFillers .selected  rolls)

    

currentDiceContainer : Model -> Html Msg
currentDiceContainer model =
    let
        attrs = 
            [ class "currentDiceContainer"] ++ currentDiceContainerStyles
        rolls =
            case model.currentRolls of
                Nothing ->
                    []
                Just r ->
                    r           
    in
        div attrs <| diePickersWithFillers (\r -> not r.selected) rolls
              


diceScorer : List PlayerRollDie -> Html Msg
diceScorer rolls =
    let
        score =
            if List.isEmpty rolls then
                []
            else
                [text <| String.fromInt <| farkleScore <| List.map .value <| List.filter .selected rolls]
    in
        span diceScorerStyles score


diePickersWithoutFillers : (PlayerRollDie -> Bool) -> List PlayerRollDie ->  List (Html Msg)
diePickersWithoutFillers selector rolls = 
        diePickers selector rolls
            |> List.filterMap identity


diePickersWithFillers : (PlayerRollDie -> Bool) -> List PlayerRollDie ->  List (Html Msg)
diePickersWithFillers selector rolls = 
    let
        unMaybe =
            (\r -> 
                case r of
                    Just element ->
                        element
                    Nothing ->
                        emptyDiePicker
            )
    in
        diePickers selector rolls
            |> List.map unMaybe

diePickers : (PlayerRollDie -> Bool) -> List PlayerRollDie ->  List (Maybe (Html Msg))
diePickers selector rolls =
    let
        pickerPicker = 
            (\i r ->
                if selector r then
                    Just (diePicker i r)
                else
                    Nothing
            )
    in
        List.indexedMap pickerPicker rolls
        

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
    , style "grid-auto-rows" dieSize
    , style "grid-auto-flow" "column"
    , style "justify-self" "center"
    , style "grid-column-gap" "20px"
    ]


diePickerStyles : List (Html.Attribute Msg)
diePickerStyles =
    [ style "display" "grid"
    , style "width" dieSize
    , style "height" dieSize
    , style "border" "1px solid darkgrey"
    , style "justify-content" "center"
    , style "align-content" "center"
    ]


diceScorerStyles : List (Html.Attribute Msg)
diceScorerStyles =
    [ style "background-color" "white"
    , style "border" "1px solid lightgrey"
    , style "justify-content" "center"
    , style "align-content" "center"
    , style "display" "grid"
    ]

dieSize : String
dieSize = 
    "60px"