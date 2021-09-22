module Pages.ExamPeriod.ExamPeriods exposing (Model, Msg, page)

import Gen.Params.ExamPeriod.ExamPeriods exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Html exposing (..)
import Api.Data exposing (Data)
import Api.ExamPeriodApi exposing (..)
import Utils.Time
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, href, class)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

type alias Model =
    {
       examPeriods : Data ExamPeriods
    }

init : (Model, Cmd Msg)
init =
    ( {
        examPeriods = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotExamPeriods
            }
        ]
    )
type Msg
  = GotExamPeriods (Data ExamPeriods) 
  |  DeletedExamPeriod (Data Int) 
  |  ClickedDeleteExamPeriod ExamPeriod

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotExamPeriods examPeriods ->
      ( { model | examPeriods = examPeriods }
        , Cmd.none
      )
    ClickedDeleteExamPeriod examPeriod ->
          ( model
          , delete
              { 
                examPeriodId = examPeriod.id
              , onResponse = DeletedExamPeriod 
              }
            )
    DeletedExamPeriod id ->
            let
                removeExamPeriod : ExamPeriods -> ExamPeriods
                removeExamPeriod =
                    List.filter (\examPeriod -> Api.Data.Success examPeriod.id /= id)
            in
            ( { model | examPeriods = Api.Data.map removeExamPeriod model.examPeriods }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Ispitni Rokovi"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-calendar-alt" ][],
              text "  Ispitni Rokovi"]
            ,div[class "mt-5"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ispitni Rok"],
                        th[class "text-center"][text "Datum Pocetka"],
                        th[class "text-center"][text "Datum Zavrsetka"],
                        th[class "text-center"][
                        a [ href (Route.toHref Route.ExamPeriod__NewExamPeriod)] [
                          button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]]
                    ], viewExamPeriods model
                ]
            ]
    ] ]
    }

viewExamPeriods : Model -> Html Msg
viewExamPeriods model =
  case model.examPeriods of
    Api.Data.Success examPeriods ->
        tbody []
        (List.map(\ep -> tr [class "text-center"][
            td[class "cursor-pointer"] [
            a[href (Route.toHref (Route.ExamPeriod__Id_ { id = String.fromInt ep.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text ep.name]]]],   
            td[class "cursor-pointer"] [
            a[href (Route.toHref (Route.ExamPeriod__Id_ { id = String.fromInt ep.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatDate ep.startDate)]]]],
            td[class "cursor-pointer"] [
            a[href (Route.toHref (Route.ExamPeriod__Id_ { id = String.fromInt ep.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatDate ep.endDate)]]]],
            td[][button[class "btn btn-danger", onClick (ClickedDeleteExamPeriod ep)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]
            ]) examPeriods)
    Api.Data.Loading ->
      tr[][
      td[][],
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"