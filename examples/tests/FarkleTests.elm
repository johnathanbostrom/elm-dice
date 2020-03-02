module FarkleTests exposing (..)
import Farkle exposing (farkleScore)
import Test exposing (..)
import Expect exposing (Expectation)



internalTests : Test
internalTests =
    describe "farkle tests"
        [ test "farkle score straight [6,2,3,4,1,5]" <|
            \_ ->
                farkleScore [6,2,3,4,1,5]
                    |> Expect.equal 1000
        , test "farkle score three pair [1,3,2,3,1,2]" <|
            \_ ->
                farkleScore [1,3,2,3,1,2]
                    |> Expect.equal 500
        , test "farkle score nothing [2,3,2,3,4,6]" <|
            \_ ->
                farkleScore [2,3,2,3,4,6]
                    |> Expect.equal 0
        , test "farkle score three twos [2,4,2,4,2,6]" <|
            \_ ->
                farkleScore [2,4,2,4,2,6]
                    |> Expect.equal 200
        , test "farkle score three ones [1,1,1,4,4,6]" <|
            \_ ->
                farkleScore [1,1,1,4,4,6]
                    |> Expect.equal 1000
        , test "farkle score ones and fives [1,1,5,5,4,6]" <|
            \_ ->
                farkleScore [1,1,5,5,4,6]
                    |> Expect.equal 300
        , test "farkle score triple ones and fives [1,5,1,5,1,5]" <|
            \_ ->
                farkleScore [1,5,1,5,1,5]
                    |> Expect.equal 1500
        ]
    