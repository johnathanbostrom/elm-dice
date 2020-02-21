module Dice exposing (Dice(..), roll, Rules(..), andThen, plus, rollExpanded, RollResult(..), DieResult)
import Random

--(Int, List Int)

type Dice = D4
    | D6
    | D8
    | D10
    | D12
    | D20
    | D100
    | DX Int 
    | DCustom (Random.Generator (List RollResult))
    | Constant Int

type Rules = DropLowest
    | CombineResults


type RollResult = OneDie DieResult
    | MultipleDice DiceResult


type alias DiceResult =
    { value: Int
    , rolls: List RollResult
    }

type alias DieResult =
    { dieType: String
    , value: Int
    }

-- roll 2 D4 |> plus 2 D8 |> result add 1
plus : Random.Generator (List Int) -> Random.Generator (List Int) -> Random.Generator (List Int)
plus diceA diceB =
    Random.map2 (++) diceA diceB
       
roll : Int -> Dice -> Random.Generator (List Int)
roll numDice dieType =
    Random.list numDice (toGenerator dieType)

dExpanded : Int -> Random.Generator RollResult
dExpanded sides = 
    Random.int 1 sides
    |> Random.map (DieResult ("D" ++ String.fromInt sides))
    |> Random.map OneDie


rollExpanded : Int -> Dice -> Random.Generator (List RollResult)
rollExpanded numDice dieType =
    Random.list numDice (toExpandedGenerator dieType)

toExpandedGenerator : Dice -> Random.Generator RollResult
toExpandedGenerator dieType =
    case dieType of
        D4 ->
            dExpanded 4
        D6 ->
            dExpanded 6
        D8 ->
            dExpanded 8
        D10 ->
            dExpanded 10
        D12 ->
            dExpanded 12
        D20 ->
            dExpanded 20
        D100 ->
            dExpanded 100
        DX sides->
            dExpanded sides 
        DCustom generator ->
            dECustom generator
        Constant val ->
            Random.constant (OneDie (DieResult "constant" val))

dECustom : Random.Generator (List RollResult) -> Random.Generator RollResult
dECustom roller =
    Random.map combineExpanded roller
    |> Random.map MultipleDice


combineExpanded : List RollResult -> DiceResult
combineExpanded rollResults = 
    let
        val = List.map (\d -> getRollValue d) rollResults |> List.foldl (+) 0
    in
        DiceResult val rollResults

getRollValue : RollResult -> Int
getRollValue rollResult =
    case rollResult of
        OneDie d -> d.value
        MultipleDice m -> m.value


andThen : Rules -> Random.Generator (List Int) -> Random.Generator (List Int)
andThen action generator =
    Random.map (handleRule action) generator

toGenerator : Dice -> Random.Generator Int
toGenerator dieType =
    case dieType of
        D4 ->
            dX 4
        D6 ->
            dX 6
        D8 ->
            dX 8
        D10 ->
            dX 10
        D12 ->
            dX 12
        D20 ->
            dX 20
        D100 ->
            dX 100
        DX sides->
            dX sides 
        DCustom generator ->
            Random.constant 1 --dCustom generator
        Constant val ->
            Random.constant val

dieName : Dice -> String
dieName dieType =
    case dieType of
        D4 ->
            "D4"
        D6 ->
            "D6"
        D8 ->
            "D8"
        D10 ->
            "D10"
        D12 ->
            "D12"
        D20 ->
            "D20"
        D100 ->
            "D100"
        DX sides->
            "D" ++ (String.fromInt sides)
        DCustom generator ->
            "Custom"
        Constant val ->
            "Constant: " ++ (String.fromInt val)


rollx : Int -> Dice -> List Rules -> Random.Generator (List Int)
rollx numDice dieType rules =
    roll numDice dieType
    |> handleRules rules


handleRules : List Rules -> Random.Generator (List Int) -> Random.Generator (List Int)
handleRules rules rollGenerator =
    List.map handleRule rules
    |>  List.foldl Random.map rollGenerator


handleRule : Rules -> (List Int -> List Int)
handleRule rule =
    case rule of
        DropLowest -> 
            dropLowest
        CombineResults -> 
            combineResults



dropLowest : List Int -> List Int
dropLowest rolls =
    rolls
    |> List.sort
    |> List.drop 1

combineResults : List Int -> List Int
combineResults rolls =
    List.foldl (+) 0 rolls
    |> List.singleton


dX : Int -> Random.Generator Int
dX sides =
    Random.int 1 sides

dCustom : Random.Generator (List Int) -> Random.Generator Int
dCustom roller =
    Random.map cR roller 


cR : List Int -> Int
cR rolls =
    List.foldl (+) 0 rolls