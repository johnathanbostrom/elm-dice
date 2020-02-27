# elm-dice  
  
A Dice Roller Package based on [elm/random](https://package.elm-lang.org/packages/elm/random/latest/) that allows you to build customizable dice rolling functions in a readable way.  

## Rolling Dice
    -- generator to roll 4 six sided dice, dropping the lowest
    statGenerator =
        roll 4 D6
            |> dropLowest


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
            |> dropLowest
            |> Random.generate GotRollResults
  
## Example Output  

### 1D6
    -- generator
    roll 1 D6

    -- one possible output
    { description = "1D6"
    , value = 4
    , children = Empty
    }  
     
### dropLowest  
the dropLowest helper function computes the value of the Roll result without the lowest value of the child rolls.
    
    --generator 
    roll 4 D6 
        |> dropLowest  
      
    -- one possible output
    { description = "4D6"
    , value = 12
    , children = 
        RollResults
            [ { description = "D6", value = 6, children = Empty }
            , { description = "D6", value = 4, children = Empty }
            , { description = "D6", value = 2, children = Empty }
            , { description = "D6", value = 1, children = Empty }
            ]
    }

### Exploding dice 
"explodes" a RollResult.  An exploding die will keep rolling as long as it satisfies the predicate. For instance, if you roll a 10 on the following D10, you will roll again and add the rolls together. If your reroll is another D10, you repeat this process.

    -- define a ten sided die that "explodes" on a 10.
    explodingD10 =
        roll 1 D10
            |> andThen ExplodeIf ((==) 10)
            |> CustomDie "Exploding D10"

    -- generator for rolling 3 exploding dice
        roll 3 explodingD10

    -- one possible output
    { description = "3 Exploding D10"
    , value = 42
    , children = 
        RollResults
            [ { description = "Exploding D10"
              , value = 27
              , children =  
                    [ { description = "D10", value = 7, children = Empty }
                    , { description = "D10", value = 10, children = Empty }
                    , { description = "D10", value = 10, children = Empty }
                    ]
              }
            , { description = "Exploding D10"
              , value = 3
              , children = 
                [ { description = "D10", value = 3, children = Empty } ] 
              }
            , { description = "Exploding D10"
              , value = 12
              , children =  
                    [ { description = "D10", value = 2, children = Empty }
                    , { description = "D10", value = 10, children = Empty }
                    ]
              }
            ]
    }       


## Count Successes  
Count the number of dice that satisfy the predicate.

    -- generator 
    roll 4 D10 
        |> countSuccessesIf (\r -> r > 7)

    -- one possible output  

    { description = "4D10"
    , value = 2
    , children = 
        RollResults
            [ { description = "D10", value = 8, children = Empty }
            , { description = "D10", value = 9, children = Empty }
            , { description = "D10", value = 2, children = Empty }
            , { description = "D10", value = 1, children = Empty }
            ]
    }