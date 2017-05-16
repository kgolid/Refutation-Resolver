module App exposing (..)

import Refutation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import List exposing (append, map, filter, concat, any, sort, head)
import String exposing (contains, filter, toUpper, toList, fromList)
import Set exposing (member, insert, empty)
import Tuple exposing (first, second)
import Maybe exposing (withDefault)
import Json.Decode as Json


type alias Model =
    { nands : List Clause
    , ors : List Clause
    , current : String
    , generated : Maybe String
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


type Msg
    = KeyDown Int
    | InputChanged String
    | ClauseClicked ClauseType String
    | ResolverClicked
    | GeneratedClicked


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        GeneratedClicked ->
            case model.generated of
                Nothing ->
                    ( model, Cmd.none )

                Just generated ->
                    ( { model | nands = add_clause_to_list generated NAND model.nands }
                    , Cmd.none
                    )

        ResolverClicked ->
            let
                nand_clauses =
                    List.map (\n -> n.term) (getSelectedNANDs model)

                maybe_or_clause =
                    getSelectedOr model
            in
                case maybe_or_clause of
                    Nothing ->
                        ( { model | generated = Nothing }, Cmd.none )

                    Just or_clause ->
                        ( { model | generated = Refutation.step nand_clauses or_clause.term }, Cmd.none )

        ClauseClicked ctype term ->
            case ctype of
                NAND ->
                    { model | nands = changeNANDStatus model.nands term } |> update ResolverClicked

                OR ->
                    { model | ors = changeORStatus model.ors term } |> update ResolverClicked

        InputChanged name ->
            ( { model | current = toUpper name }, Cmd.none )

        KeyDown code ->
            if (code == 13) then
                if contains "@" model.current then
                    let
                        term =
                            String.filter (\s -> s /= '@') model.current
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
      , generated = Nothing
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
            list ++ [ (new_clause term t) ]


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
                , value model.current
                ]
                []
            ]
        , div
            []
            [ display_list_component model.ors "or_clauses"
            , display_list_component model.nands "nand_clauses"
            ]
        , div
            [ class "result" ]
            (display_result_components model.nands model.generated)
        ]


onKeyDown : (Int -> a) -> Attribute a
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


display_result_components : List Clause -> Maybe String -> List (Html Msg)
display_result_components list maybe_term =
    case maybe_term of
        Nothing ->
            []

        Just term ->
            let
                class_name =
                    if contains_clause term list then
                        "clause"
                    else
                        "new_clause clause"
            in
                [ p [ onClick (GeneratedClicked), class class_name ] [ text (display_term term) ] ]


display_list_component : List Clause -> String -> Html Msg
display_list_component list class_name =
    div [ class ("clause_list " ++ class_name) ]
        (List.map (\c -> p [ onClick (ClauseClicked c.ctype c.term), class ("clause " ++ (toString c.status)) ] [ text (display_term c.term) ]) list)


display_term : String -> String
display_term term =
    if term == "" then
        "Ø"
    else
        term



-- UTILS


getSelectedNANDs : Model -> List Clause
getSelectedNANDs model =
    List.filter (\n -> n.status == Selected) model.nands


getSelectedOr : Model -> Maybe Clause
getSelectedOr model =
    head (List.filter (\o -> o.status == Selected) model.ors)


getClauseList : Model -> ClauseType -> List Clause
getClauseList model ctype =
    case ctype of
        OR ->
            model.ors

        NAND ->
            model.nands


changeNANDStatus : List Clause -> String -> List Clause
changeNANDStatus list t =
    case list of
        [] ->
            []

        x :: xs ->
            if (x.term == t) then
                { x | status = statusChangeOnClick x.status } :: xs
            else
                x :: (changeNANDStatus xs t)


changeORStatus : List Clause -> String -> List Clause
changeORStatus list t =
    case list of
        [] ->
            []

        x :: xs ->
            if (x.term == t) then
                { x | status = statusChangeOnClick x.status } :: (changeORStatus xs t)
            else if (x.status == Selected) then
                { x | status = Selectable } :: (changeORStatus xs t)
            else
                x :: (changeORStatus xs t)


statusChangeOnClick : ClauseStatus -> ClauseStatus
statusChangeOnClick status =
    case status of
        Selectable ->
            Selected

        Selected ->
            Selectable


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
