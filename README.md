# elm-dice  
  
A Dice Roller Package based on [elm/random](https://package.elm-lang.org/packages/elm/random/latest/) that allows you to build customizable dice rolling functions in a readable way.  

## Rolling Dice
    --roll 4 six sided dice, dropping the lowest:
    roll4D6 =
        roll 4 D6
            |> andThen DropLowest


## Example App
    type alias Model =
    { rolls : List RollResult
    }

    type Msg
        = RollDice
        | GotRollResults (List RollResult)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            RollDice ->
                ( model
                , rollStats
                )

            GotRollResults newRolls ->
                ( { model | rolls = newRolls }
                , Cmd.none
                )

    rollstats : Cmd Msg
    rollstats =
        roll 4 D6
            |> andThen DropLowest
            |> Random.generate GotRollResults
