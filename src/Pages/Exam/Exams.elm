module Pages.Exam.Exams exposing (Model, Msg, page)

import Gen.Params.Exam.Exams exposing (Params)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Html exposing (..)
import Html.Attributes exposing (class)
import Api.Data exposing (Data)
import Api.ExamApi exposing (..)
import Utils.Time
import Api.StudentApi exposing (Student)
import Html.Events exposing (onClick)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- INIT
type alias Model =
    { 
      exams : Data Exams
    }

init : (Model, Cmd Msg)
init =
    ( {
        exams = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotExams
            }
        ]
    )

-- UPDATE
type Msg
  = GotExams (Data Exams) 
  | DeletedExams (Data Int) 
  | ClickedDeleteExams Exam

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotExams exams ->
      ( { model | exams = exams }
        , Cmd.none
      )
    ClickedDeleteExams exam ->
          ( model
          , delete
              { 
                examId = exam.id
              , onResponse = DeletedExams 
              }
            )
    DeletedExams id ->
            let
                removeExam : Exams -> Exams
                removeExam =
                    List.filter (\exam -> Api.Data.Success exam.id /= id)
            in
            ( { model | exams = Api.Data.map removeExam model.exams }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Ispiti"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-graduation-cap" ][]
              ,text " Ispiti"]
            ,div[class "mt-5"][
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ispitni Rok"],
                        th[class "text-center"][text "Predmet"],
                        th[class "text-center"][text "Poeni sa Ispita"],
                        th[class "text-center"][text "Poeni sa Predispitnih Obaveza"],
                        th[class "text-center"][text "Vreme Odrzavanja Ispita"],
                        th[class "text-center"][text "Ime i Prezime Studenta"],
                        th[class "text-center"][text "Br Indeksa"],
                        th[class "text-center"][button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                    ], viewExams model
                ]
            ]
    ] ]
    }

viewExams : Model -> Html Msg
viewExams model =
  case model.exams of
    Api.Data.Success exams ->
        tbody []
        (List.map(\e -> tr [class "text-center"][
            td[] [text e.examPeriod.name],
            td[] [text e.course.name],  
            td[] [text (String.fromInt e.examPoints)],
            td[] [text (String.fromInt e.labPoints)],
            td[] [text (Utils.Time.formatDate e.date)],
            td[] [text (studentNameText e.student)],
            td[] [text (studentCardText e.student)],

            td[][button[class "btn btn-danger", onClick (ClickedDeleteExams e)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]
            ]) exams)
    Api.Data.Loading ->
      tr[][
      td[][],
      td[][],
      td[][],
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"

studentNameText: Maybe Student -> String
studentNameText student =
  case student of
    Nothing -> ""
    Just value -> value.firstName ++ " " ++ value.lastName

studentCardText: Maybe Student -> String
studentCardText student =
  case student of
    Nothing -> ""
    Just value -> value.cardNumber