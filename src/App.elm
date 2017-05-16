module App exposing (..)

import Refutation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (append, map, filter, concat, any, sort, head)
import String exposing (contains, toUpper, toList, fromList)
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
    | ClauseClicked Clause
    | GeneratedClicked


init : ( Model, Cmd a )
init =
    ( { nands = []
      , ors = []
      , current = ""
      , generated = Nothing
      }
    , Cmd.none
    )


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
                    model |> add_clause_to_list generated NAND |> unselectAllClauses |> updateGenerated |> addCmd

        ClauseClicked clause ->
            case clause.ctype of
                NAND ->
                    model |> changeNANDStatus clause.term |> updateGenerated |> addCmd

                OR ->
                    model |> changeORStatus clause.term |> updateGenerated |> addCmd

        InputChanged name ->
            { model | current = toUpper name } |> addCmd

        KeyDown code ->
            if (code /= 13) then
                ( model, Cmd.none )
            else if contains "\\" model.current then
                let
                    term =
                        String.filter ((/=) '\\') model.current
                in
                    model |> add_clause_to_list term OR |> clearCurrent |> addCmd
            else
                model |> add_clause_to_list model.current NAND |> clearCurrent |> addCmd



-- Model Manipulations


add_clause_to_list : String -> ClauseType -> Model -> Model
add_clause_to_list s t model =
    let
        term =
            (fromList << sort << dropDuplicates << toList) s

        list =
            ite (t == NAND) model.nands model.ors
    in
        if contains_clause term list then
            model
        else if t == NAND then
            { model | nands = list ++ [ (new_clause term t) ] }
        else
            { model | ors = list ++ [ (new_clause term t) ] }


contains_clause : String -> List Clause -> Bool
contains_clause s list =
    any (\c -> c.term == s) list


new_clause : String -> ClauseType -> Clause
new_clause s t =
    { term = s
    , ctype = t
    , status = Selectable
    }


unselectAllClauses : Model -> Model
unselectAllClauses model =
    { model
        | nands = List.map (\c -> { c | status = Selectable }) model.nands
        , ors = List.map (\c -> { c | status = Selectable }) model.ors
    }


clearCurrent : Model -> Model
clearCurrent model =
    { model | current = "" }


updateGenerated : Model -> Model
updateGenerated model =
    let
        maybe_or_clause =
            getSelectedOR model
    in
        case maybe_or_clause of
            Nothing ->
                { model | generated = Nothing }

            Just or_clause ->
                let
                    nand_clauses =
                        List.map .term (getSelectedNANDs model)
                in
                    { model | generated = Refutation.step nand_clauses or_clause.term }


getSelectedNANDs : Model -> List Clause
getSelectedNANDs model =
    filter (\n -> n.status == Selected) model.nands


getSelectedOR : Model -> Maybe Clause
getSelectedOR model =
    head (filter (\o -> o.status == Selected) model.ors)


changeNANDStatus : String -> Model -> Model
changeNANDStatus t model =
    let
        updated =
            List.map (\n -> ite (n.term == t) (toggleStatus n) n) model.nands
    in
        { model | nands = updated }


changeORStatus : String -> Model -> Model
changeORStatus t model =
    let
        updated =
            List.map (\n -> ite (n.term == t) (toggleStatus n) { n | status = Selectable }) model.ors
    in
        { model | ors = updated }


toggleStatus : Clause -> Clause
toggleStatus clause =
    case clause.status of
        Selectable ->
            { clause | status = Selected }

        Selected ->
            { clause | status = Selectable }



-- View


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
            if contains_clause term list then
                [ p [ class "clause" ] [ text (display_term term) ] ]
            else
                [ p [ onClick (GeneratedClicked), class "new_clause clause" ] [ text (display_term term) ] ]


display_list_component : List Clause -> String -> Html Msg
display_list_component list class_name =
    div [ class ("clause_list " ++ class_name) ]
        (List.map (\c -> p [ onClick (ClauseClicked c), class ("clause " ++ (toString c.status)) ] [ text (display_term c.term) ]) list)


display_term : String -> String
display_term term =
    if term == "" then
        "Ø"
    else
        term



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
        List.foldl step ( Set.empty, [] ) list |> second |> List.reverse


ite : Bool -> a -> a -> a
ite b x y =
    if b then
        x
    else
        y


addCmd : a -> ( a, Cmd b )
addCmd x =
    ( x, Cmd.none )
