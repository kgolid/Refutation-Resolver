module App exposing (..)

import Refutation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import List exposing (append, map, filter, concat, any, sort, head)
import String exposing (contains, filter, toUpper, toList, fromList)
import Set exposing (member, insert, empty)
import Maybe exposing (withDefault)
import Json.Decode as Json


type alias Model =
    { nands : List Clause
    , ors : List Clause
    , current : String
    , generated : String
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
    | ClauseClicked ClauseType String
    | ResolverClicked


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        ResolverClicked ->
            let
                nand_clauses =
                    map (\n -> n.term) (getSelectedNANDs model)

                maybe_or_clause =
                    getSelectedOr model
            in
                case maybe_or_clause of
                    Nothing ->
                        ( model, Cmd.none )

                    Just or_clause ->
                        ( { model | generated = Refutation.step nand_clauses or_clause.term }, Cmd.none )

        ClauseClicked ctype term ->
            case ctype of
                NAND ->
                    ( { model | nands = changeStatus model.nands term }, Cmd.none )

                OR ->
                    ( { model | ors = changeStatus model.ors term }, Cmd.none )

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
      , generated = ""
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
        , div
            []
            [ button [ onClick ResolverClicked ] [ text "Resolve!" ]
            , p [] [ text model.generated ]
            ]
        ]


onKeyDown : (Int -> a) -> Attribute a
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


display_list_component : List Clause -> String -> String -> Html Msg
display_list_component list title class_name =
    div [ class ("clause_list " ++ class_name) ]
        [ h3 [] [ text title ]
        , div [] (map (\c -> p [ onClick (ClauseClicked c.ctype c.term), class (toString c.status) ] [ text c.term ]) list)
        ]



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


changeStatus : List Clause -> String -> List Clause
changeStatus list t =
    case list of
        [] ->
            []

        x :: xs ->
            if (x.term == t) then
                { x | status = statusChangeOnClick x.status } :: xs
            else
                x :: (changeStatus xs t)


statusChangeOnClick : ClauseStatus -> ClauseStatus
statusChangeOnClick status =
    case status of
        Unselectable ->
            Unselectable

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
        List.foldl step ( Set.empty, [] ) list |> snd |> List.reverse
