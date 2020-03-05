module Helpers exposing (allMaxBy, allMinBy)

allMaxBy : (a -> comparable) -> List a -> List a
allMaxBy toComparator list =
    let
        fn = (\i li -> 
                case li of
                    [] ->
                        [i]
                    x :: xs ->
                        case compare (toComparator i) (toComparator x) of
                            LT -> 
                                li
                            EQ ->
                                i :: li
                            GT ->
                                [i]
             )
    in
        List.foldl fn [] list

allMinBy : (a -> comparable) -> List a -> List a
allMinBy toComparator list =
    let
        fn = (\i li -> 
                case li of
                    [] ->
                        [i]
                    x :: xs ->
                        case compare (toComparator i) (toComparator x) of
                            LT -> 
                                [i]
                            EQ ->
                                i :: li
                            GT ->
                                li
             )
    in
        List.foldl fn [] list

