module FuzzTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, custom)
import Shrink
import Random
import Test exposing (..)
import Dice exposing (..)
import List.Extra exposing (dropWhile, takeWhile)


oneDieTests : Test
oneDieTests =
    describe "One Die"
    [ describe "One D4"
            [ fuzz (rollFuzz (roll 1 D4)) "rolls 1 D4, at most 4" <|
                \die ->
                    Expect.atMost 4 die.value
            ,  fuzz (rollFuzz (roll 1 D4)) "rolls 1 D4, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D4)) "rolls 1 D4, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D4)) "rolls 1 D4, description D4" <|
                \die ->
                    Expect.equal "D4" die.description
            ]
    ,  describe "One D6"
            [ fuzz (rollFuzz (roll 1 D6)) "rolls 1 D6, at most 6" <|
                \die ->
                    Expect.atMost 6 die.value
            ,  fuzz (rollFuzz (roll 1 D6)) "rolls 1 D6, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D6)) "rolls 1 D6, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D6)) "rolls 1 D6, description D6" <|
                \die ->
                    Expect.equal "D6" die.description
            ]
    ,  describe "One D8"
            [ fuzz (rollFuzz (roll 1 D8)) "rolls 1 D8, at most 8" <|
                \die ->
                    Expect.atMost 8 die.value
            ,  fuzz (rollFuzz (roll 1 D8)) "rolls 1 D8, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D8)) "rolls 1 D8, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D8)) "rolls 1 D8, description D8" <|
                \die ->
                    Expect.equal "D8" die.description
            ]
    ,  describe "One D10"
            [ fuzz (rollFuzz (roll 1 D10)) "rolls 1 D10, at most 10" <|
                \die ->
                    Expect.atMost 10 die.value
            ,  fuzz (rollFuzz (roll 1 D10)) "rolls 1 D10, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D10)) "rolls 1 D10, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D10)) "rolls 1 D10, description D10" <|
                \die ->
                    Expect.equal "D10" die.description
            ]
    ,  describe "One D12"
            [ fuzz (rollFuzz (roll 1 D12)) "rolls 1 D12, at most 12" <|
                \die ->
                    Expect.atMost 12 die.value
            ,  fuzz (rollFuzz (roll 1 D12)) "rolls 1 D12, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D12)) "rolls 1 D12, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D12)) "rolls 1 D12, description D12" <|
                \die ->
                    Expect.equal "D12" die.description
            ]
    ,  describe "One D20"
            [ fuzz (rollFuzz (roll 1 D20)) "rolls 1 D20, at most 20" <|
                \die ->
                    Expect.atMost 20 die.value
            ,  fuzz (rollFuzz (roll 1 D20)) "rolls 1 D20, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D20)) "rolls 1 D20, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D20)) "rolls 1 D20, description D20" <|
                \die ->
                    Expect.equal "D20" die.description
            ]
    ,  describe "One D100"
            [ fuzz (rollFuzz (roll 1 D100)) "rolls 1 D100, at most 100" <|
                \die ->
                    Expect.atMost 100 die.value
            ,  fuzz (rollFuzz (roll 1 D100)) "rolls 1 D100, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 D100)) "rolls 1 D100, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 D100)) "rolls 1 D100, description D100" <|
                \die ->
                    Expect.equal "D100" die.description
            ]
    ,  describe "One DX"
            [ fuzz (rollFuzz (roll 1 <| DX 13)) "rolls 1 D13, at most 13" <|
                \die ->
                    Expect.atMost 13 die.value
            ,  fuzz (rollFuzz (roll 1 <| DX 13)) "rolls 1 D13, at least 1" <|
                \die ->
                    Expect.atLeast 1 die.value
            ,  fuzz (rollFuzz (roll 1 <| DX 13)) "rolls 1 D13, children Empty" <|
                \die ->
                    Expect.equal Empty die.children
            ,  fuzz (rollFuzz (roll 1 <| DX 13)) "rolls 1 D3, description D13" <|
                \die ->
                    Expect.equal "D13" die.description
            ]
    ]

customDieTests : Test
customDieTests =
    describe "Custom Die"
        [ describe "Custom All the Same"
                [ fuzz (rollFuzz (roll 1 allTwos)) "rolls 1 allTwos, always 2" <|
                    \die ->
                        Expect.equal 2 die.value
                ,  fuzz (rollFuzz (roll 1 allTwos)) "rolls 1 allTwos, children Empty" <|
                    \die ->
                        Expect.equal Empty die.children
                ,  fuzz (rollFuzz (roll 1 allTwos)) "rolls 1 allTwos, description allTwos" <|
                    \die ->
                        Expect.equal "allTwos" die.description
                ]
        , describe "Weighted All the Same"
                [ fuzz (rollFuzz (roll 1 allTwosWeighted)) "rolls 1 allTwos, always 2" <|
                    \die ->
                        Expect.equal 2 die.value
                ,  fuzz (rollFuzz (roll 1 allTwosWeighted)) "rolls 1 allTwos, children Empty" <|
                    \die ->
                        Expect.equal Empty die.children
                ,  fuzz (rollFuzz (roll 1 allTwosWeighted)) "rolls 1 allTwos, description allTwos" <|
                    \die ->
                        Expect.equal "allTwos" die.description
                ]
        ]

simpleDicePoolTests : Test
simpleDicePoolTests =
    describe "Dice Pool"
            [ fuzz (rollFuzz (roll 2 D6)) "rolls 2 D6, at most 12" <|
                \die ->
                    Expect.atMost 12 die.value
            ,  fuzz (rollFuzz (roll 2 D6)) "rolls 2 D6, at least 2" <|
                \die ->
                    Expect.atLeast 2 die.value
            ,  fuzz (rollFuzz (roll 2 D6)) "rolls 2 D6, has two children" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            Expect.equal 2 <| List.length children
            ,  fuzz (rollFuzz (roll 2 D6)) "rolls 2 D6, value is sum of child rolls" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            Expect.equal die.value <| List.foldl (+) 0 <| List.map .value children 
            ,  fuzz (rollFuzz (roll 2 D6)) "rolls 2 D6, description 2 D6" <|
                \die ->
                    Expect.equal "2 D6" die.description
            ]

dropLowestTests : Test
dropLowestTests =
    describe "Drop Lowest"
            [ fuzz (rollFuzz (roll 4 D6 |> dropLowest)) "rolls 4 D6 drop lowest, at most 18" <|
                \die ->
                    Expect.atMost 18 die.value
            ,  fuzz (rollFuzz (roll 4 D6 |> dropLowest)) "rolls 4 D6 drop lowest, at least 3" <|
                \die ->
                    Expect.atLeast 3 die.value
            ,  fuzz (rollFuzz (roll 4 D6 |> dropLowest)) "rolls 4 D6 drop lowest, has four children" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            Expect.equal 4 <| List.length children
            ,  fuzz (rollFuzz (roll 4 D6 |> dropLowest)) "rolls 4 D6 drop lowest, value is sum of child rolls sans lowest" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            let
                                expected = 
                                    List.map .value children
                                    |> List.sort
                                    |> List.tail
                                    |> Maybe.withDefault []
                                    |> List.foldl (+) 0
                            in
                                Expect.equal die.value expected
            ,  fuzz (rollFuzz (roll 4 D6 |> dropLowest)) "rolls 4 D6 drop lowest, description 4 D6" <|
                \die ->
                    Expect.equal "4 D6" die.description
            ]

explodingDiceTests : Test
explodingDiceTests =
    describe "Exploding dice"
            [ fuzz (rollFuzz (roll 10 explodingD10)) "exploding d10s, at least 10" <|
                \die ->
                    Expect.atLeast 10 die.value
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "exploding d10s, at least 10 children" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            Expect.atLeast 10 <| List.length children
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "exploding d10s, value is sum of child rolls" <|
                \die ->
                    case die.children of
                        Empty ->
                            Expect.fail "No children"
                        RollResults children ->
                            let
                                expected = 
                                    List.map .value children
                                    |> List.foldl (+) 0
                            in
                                Expect.equal die.value expected
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "exploding d10s, has grandchildren" <|
                \die ->
                    getGrandChildren die
                    |> List.length
                    |> Expect.atLeast 10
            ,  fuzz (rollFuzz fiftyExplodingD4s) "exploding d4s, some 4s exploded" <|
                \die ->
                    Expect.greaterThan 50 <| List.length <| List.concat <| getGrandChildren die   
            ,  fuzz (rollFuzz fiftyExplodingD4s) "exploding d4s, 4s valid explosions" <|
                \die ->
                    getGrandChildren die 
                        |> List.any (hasValidExplodedResults (\r -> r.value == 4)) 
                        |> Expect.true "invalid exploded results"  
            ,  fuzz (rollFuzz foreverExploding) "forever exploding dice, will stop exploding at 100" <|
                \die ->
                    getGrandChildren die
                    |> List.all (\x -> List.length x == 100)
                    |> Expect.false "Expected that all dice would explode 100 times" 
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "roll exploding dice, value should be sum of child values" <|
                \die ->
                    valueOfChildren die
                    |> Expect.equal die.value
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "roll exploding dice, value of exploding children should be sum of their child values" <|
                \die ->
                    getChildren die
                    |> List.filter (\c -> List.length (getChildren c) > 1)
                    |> List.all (\c -> c.value == valueOfChildren c)
                    |> Expect.true "Expected all exploded children to have a value equal to their children"
            ,  fuzz (rollFuzz (roll 10 explodingD10)) "exploding d10s, description 10 exploding D10" <|
                \die ->
                    Expect.equal "10 exploding D10" die.description
            ]

plusTests : Test
plusTests = describe "plus tests"
                [ fuzz (rollFuzz (roll 1 D6 |> plus (roll 1 D4))) "rolls 1 D6 plus 1 D4, at most 10" <|
                    \die ->
                        Expect.atMost 10 die.value
                ,  fuzz (rollFuzz (roll 1 D6 |> plus (roll 1 D4))) "rolls 1 D6 plus 1 D4, at least 2" <|
                    \die ->
                        Expect.atLeast 1 die.value
                ,  fuzz (rollFuzz (roll 1 D6 |> plus (roll 1 D4))) "rolls 1 D6 plus 1 D4, two children" <|
                    \die ->
                       case die.children of
                            Empty ->
                                Expect.fail "No children"
                            RollResults children ->
                                Expect.equal 2 <| List.length children
                ,  fuzz (rollFuzz (roll 1 D6 |> plus (roll 1 D4))) "rolls 1 D6 plus 1 D4, value is sum of child rolls" <|
                    \die ->
                        case die.children of
                            Empty ->
                                Expect.fail "No children"
                            RollResults children ->
                                Expect.equal die.value <| List.sum <| List.map .value children 
                ,  fuzz (rollFuzz (roll 1 D6 |> plus (roll 1 D4))) "rolls 1 D6 plus 1 D4, description D4 + D6" <|
                    \die ->
                        Expect.equal "D4 + D6" die.description
                ,  fuzz (rollFuzz (roll 2 D6 |> plus (roll 2 D4))) "rolls 2 D6 plus 2 D4, value is sum of child rolls" <|
                    \die ->
                        case die.children of
                            Empty ->
                                Expect.fail "No children"
                            RollResults children ->
                                Expect.equal die.value <| List.sum <| List.map .value children 
                ,  fuzz (rollFuzz (roll 2 D6 |> plus (roll 2 D4))) "rolls 2 D6 plus 2 D4, description 2D4 + 2D6" <|
                    \die ->
                        Expect.equal "2 D4 + 2 D6" die.description
                ]

internalTests : Test
internalTests = describe "test helpers"
                    [ test "hasValidExplodedResults false for empty list" <|
                        \_ ->
                            hasValidExplodedResults (\x -> True) []
                            |> Expect.false "Expected false for empty list"
                    , test "hasValidExplodedResults false for singleton list" <|
                        \_ ->
                            hasValidExplodedResults (\x -> True) [RollResult "test" 1 Empty]
                            |> Expect.false "Expected false for singleton list"
                    , test "hasValidExplodedResults true for valid list" <|
                        \_ ->
                            hasValidExplodedResults (\x -> x.value == 4) sampleValidExplosions
                            |> Expect.true "Expected true for valid list"
                    , test "hasValidExplodedResults false for invalid list" <|
                        \_ ->
                            hasValidExplodedResults (\x -> x.value == 4) sampleInvertedExplosions
                            |> Expect.false "Expected false for invalid list"
                    ]


valueOfChildren : RollResult -> Int
valueOfChildren roll =
    getChildren roll
        |> List.map .value
        |> List.sum


rollFuzz : Random.Generator RollResult -> Fuzzer RollResult
rollFuzz generator = 
    custom generator Shrink.noShrink  --TODO: better shrink

allTwos : Dice
allTwos =
    CustomDie "allTwos" [ 2, 2, 2, 2, 2]

allTwosWeighted : Dice
allTwosWeighted =
    WeightedDie "allTwos" [ (1, 2) ]

explodingD10 : Dice
explodingD10 =
    roll 1 D10 
        |> explodeIf ((==) 10)
        |> CompoundDie "exploding D10"


getGrandChildren : RollResult -> List (List RollResult)
getGrandChildren rollResult =
    getChildren rollResult
    |> List.map getChildren 


getChildren : RollResult -> List RollResult
getChildren rollResult =
    case rollResult.children of
        Empty ->
            []
        RollResults children ->
            children


fiftyExplodingD4s : Random.Generator RollResult
fiftyExplodingD4s =
    roll 1 D4 
        |> explodeIf ((==)4)
        |> CompoundDie "exploding D4"
        |>  roll 50

foreverExploding : Random.Generator RollResult
foreverExploding =
    roll 1 D4
        |> explodeIf (\_ -> True)

hasValidExplodedResults : (RollResult -> Bool) -> List RollResult  -> Bool
hasValidExplodedResults test explodedResults =
    case explodedResults of
        [] -> 
            False
        x::[] -> 
            False
        _ -> 
            List.reverse explodedResults
            |> List.Extra.dropWhile test
            |> List.length
            |> (==) 1

sampleValidExplosions : List RollResult
sampleValidExplosions =
    [ RollResult "" 2 Empty 
    , RollResult "" 4 Empty
    , RollResult "" 4 Empty
    , RollResult "" 4 Empty
    ]

sampleInvertedExplosions : List RollResult
sampleInvertedExplosions =
    [ RollResult "" 4 Empty 
    , RollResult "" 4 Empty
    , RollResult "" 4 Empty
    , RollResult "" 2 Empty
    ]
    