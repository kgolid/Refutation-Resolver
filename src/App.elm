module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import List exposing (append, map, concat, any, sort)
import String exposing (contains, filter, toUpper, toList, fromList)
import Set exposing (member, insert, empty)
import Json.Decode as Json


type alias Model =
    { nands : List Clause
    , ors : List Clause
    , current : String
    }


type alias Clause =
    { term : String
    , ctype : ClauseType
    , status : ClauseStatus
    }


type ClauseType
    = OR
    | NAND


type ClauseStatus
    = Selected
    | Selectable
    | Unselectable


type Msg
    = KeyDown Int
    | InputChanged String
    | ClauseSelected ClauseType String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        ClauseSelected t n ->
            ( model, Cmd.none )

        InputChanged name ->
            ( { model | current = toUpper name }, Cmd.none )

        KeyDown code ->
            if (code == 13) then
                if contains "@" model.current then
                    let
                        term =
                            filter (\s -> s /= '@') model.current
                    in
                        ( { model
                            | ors = add_clause_to_list term OR model.ors
                            , current = ""
                          }
                        , Cmd.none
                        )
                else
                    ( { model
                        | nands = add_clause_to_list model.current NAND model.nands
                        , current = ""
                      }
                    , Cmd.none
                    )
            else
                ( model, Cmd.none )


init : ( Model, Cmd a )
init =
    ( { nands = []
      , ors = []
      , current = ""
      }
    , Cmd.none
    )


new_clause : String -> ClauseType -> Clause
new_clause s t =
    { term = s
    , ctype = t
    , status = Selectable
    }


add_clause_to_list : String -> ClauseType -> List Clause -> List Clause
add_clause_to_list s t list =
    let
        term =
            (fromList << sort << dropDuplicates << toList) s
    in
        if contains_clause term list then
            list
        else
            (new_clause term t) :: list


contains_clause : String -> List Clause -> Bool
contains_clause s list =
    any (\c -> c.term == s) list


view : Model -> Html Msg
view model =
    section [ class "main_section" ]
        [ div
            []
            [ input
                [ onInput InputChanged
                , onKeyDown KeyDown
                , placeholder "Enter axioms here"
                , value model.current
                ]
                []
            ]
        , div
            []
            [ display_list_component model.nands "NANDs" "nand_clauses"
            , display_list_component model.ors "ORs" "or_clauses"
            ]
        ]


onKeyDown : (Int -> a) -> Attribute a
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


display_list_component : List Clause -> String -> String -> Html Msg
display_list_component list title class_name =
    div [ class ("clause_list " ++ class_name) ]
        [ h3 [] [ text title ]
        , div [] (map (\c -> p [ class (toString c.status) ] [ text c.term ]) list)
        ]



-- UTILS


dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )
            else
                ( Set.insert next set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list |> snd |> List.reverse
