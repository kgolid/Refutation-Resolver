module Refutation exposing (step)

import String exposing (toList, fromList, concat, fromChar, contains)
import List exposing (map, map2, all, concat, filter, concatMap, head, foldl, reverse)
import Tuple exposing (second)
import Set exposing (member, insert, empty)
import Maybe exposing (withDefault)


type alias NAND =
    String


type alias OR =
    String


step : List NAND -> OR -> Maybe NAND
step ncs oc =
    let
        aligned_nands =
            head (filter (\perm -> aligns perm oc) (permutations ncs))
    in
        case aligned_nands of
            Nothing ->
                Nothing

            Just nands ->
                Just (alignedStep nands oc)


aligns : List NAND -> OR -> Bool
aligns ncs oc =
    let
        olist =
            map fromChar (toList oc)
    in
        List.length olist == List.length ncs && all identity (map2 contains olist ncs)


alignedStep : List NAND -> OR -> NAND
alignedStep ncs oc =
    let
        olist =
            toList oc
    in
        fromList (dropDuplicates (toList (String.concat (map2 removeFromString ncs olist))))


removeFromString : String -> Char -> String
removeFromString s c =
    fromList (removeFromList (toList s) c)


removeFromList : List a -> a -> List a
removeFromList list c =
    case list of
        [] ->
            []

        x :: cs ->
            if x == c then
                cs
            else
                x :: removeFromList cs c


permutations : List a -> List (List a)
permutations xss =
    case xss of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    map ((::) y) (permutations ys)
            in
                concatMap f (select xs)


select : List a -> List ( a, List a )
select xs =
    case xs of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: map (\( y, ys ) -> ( y, x :: ys )) (select xs)


dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list |> second |> List.reverse
