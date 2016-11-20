module Refutation exposing (..)

import String exposing (toList, fromList, concat, fromChar, contains)
import List exposing (map, map2, all, concat, filter, concatMap, head)
import Maybe exposing (withDefault)


type alias NAND =
    String


type alias OR =
    String


step : List NAND -> OR -> NAND
step ncs oc =
    alignedStep (withDefault [] (head (filter (\perm -> aligns perm oc) (permutations ncs)))) oc


aligns : List NAND -> OR -> Bool
aligns ncs oc =
    let
        olist =
            map fromChar (toList oc)
    in
        all identity (map2 contains olist ncs)


alignedStep : List NAND -> OR -> NAND
alignedStep ncs oc =
    let
        olist =
            toList oc
    in
        String.concat (map2 removeFromString ncs olist)


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
permutations xs' =
    case xs' of
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
