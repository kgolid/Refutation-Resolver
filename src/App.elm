module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import List exposing (append, map, concat)


type alias Model =
    { nand_container : ClauseList
    , or_container : ClauseList
    }


type ClauseType
    = Nand
    | Or


type alias ClauseList =
    { values : List String, current : String, selected_values : List String }


type Msg
    = AddToList Clause
    | AddToCurrent Clause String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    let
        mod =
            log "model" model
    in
        case msg of
            AddToList clause ->
                case clause of
                    Nand ->
                        ( if model.current_nand /= "" then
                            { model
                                | nand_container = model.current_nand :: model.nands
                                , current_nand = ""
                            }
                          else
                            model
                        , Cmd.none
                        )

                    Or ->
                        ( if model.current_or /= "" then
                            { model
                                | ors = model.current_or :: model.ors
                                , current_or = ""
                            }
                          else
                            model
                        , Cmd.none
                        )

            AddToCurrent clause name ->
                case clause of
                    Nand ->
                        ( { model | nand_container.current = name }, Cmd.none )

                    Or ->
                        ( { model | or_container.current = name }, Cmd.none )


init : ( Model, Cmd a )
init =
    ( { nand_container =
            { values = []
            , selected_values = []
            , current = ""
            }
      , or_container =
            { values = []
            , selected_values = []
            , current = ""
            }
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Axioms" ]
        , display_list_component model "NAND-clauses" Nand
        , display_list_component model "OR-clauses" Or
        ]


display_list_component : Model -> String -> ClauseType -> Html Msg
display_list_component model title clause =
    let
        list =
            get_list model clause

        current =
            get_current model clause
    in
        div [ class "clause_list" ]
            [ p [] [ text title ]
            , input [ onInput (AddToCurrent clause), placeholder (toString clause), value current ] []
            , button [ onClick (AddToList clause) ] [ text "Add clause" ]
            , div [] (map (\n -> p [] [ text n ]) list)
            ]


get_list : Model -> ClauseType -> List String
get_list model clause =
    case clause of
        Nand ->
            model.nand_container.values

        Or ->
            model.or_container.values


get_current : Model -> ClauseType -> String
get_current model clause =
    case clause of
        NandClause ->
            model.nand_container.current

        OrClause ->
            model.or_container.current
