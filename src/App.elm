module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import List exposing (append, map, concat)


type alias Model =
    { nands : List String
    , ors : List String
    , current_nand : String
    , current_or : String
    }


type ClauseType
    = Nand
    | Or


type Msg
    = AddToList ClauseType
    | AddToCurrent ClauseType String


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
                                | nands = model.current_nand :: model.nands
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
                        ( { model | current_nand = name }, Cmd.none )

                    Or ->
                        ( { model | current_or = name }, Cmd.none )


init : ( Model, Cmd a )
init =
    ( { nands = []
      , ors = []
      , current_nand = ""
      , current_or = ""
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
            model.nands

        Or ->
            model.ors


get_current : Model -> ClauseType -> String
get_current model clause =
    case clause of
        Nand ->
            model.current_nand

        Or ->
            model.current_or
