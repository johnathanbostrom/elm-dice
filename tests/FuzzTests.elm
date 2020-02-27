module FuzzTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, custom)
import Shrink
import Random
import Test exposing (..)
import Dice exposing (..)


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

rollFuzz : Random.Generator RollResult -> Fuzzer RollResult
rollFuzz generator = 
    custom generator Shrink.noShrink  --TODO: better shrink

allTwos : Dice
allTwos =
    CustomDie "allTwos" [ 2, 2, 2, 2, 2]

allTwosWeighted : Dice
allTwosWeighted =
    WeightedDie "allTwos" [ (1, 2) ]

