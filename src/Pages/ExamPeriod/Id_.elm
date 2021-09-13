module Pages.ExamPeriod.Id_ exposing (Model, Msg, page)

import Gen.Params.ExamPeriod.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Html.Attributes exposing (class, type_ , disabled, value)
import Gen.Route as Route exposing (Route)
import Html.Events exposing (onClick, onInput)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Utils.Time
import Request exposing (Request)
import Api.CourseApi exposing (Course)
import Api.ExamApi exposing (Exams)
import Api.ExamPeriodApi
import Api.ExamPeriodApi exposing (ExamPeriod)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- INIT
type alias Model =
    {
        id: String,
        name: String,
        start: String,
        end: String,
        exams: Data Exams,
        ep: Maybe (Data Course),
        examPeriod: Data ExamPeriod
    }

init : Params -> ( Model, Cmd Msg )
init params =
    ( {id = params.id, name = "", start = "", end = "", ep = Nothing, examPeriod = Api.Data.Loading, exams = Api.Data.Loading}, 
    Cmd.batch
        [ Api.ExamApi.getExamPeriodExams
            { 
              examPeriodId = params.id,
              onResponse = GotExams
            },
         Api.ExamPeriodApi.getById
            { 
              examPeriodId = params.id,
              onResponse = GotExamPeriod 
            }
        ]
    )

-- UPDATE
type Msg
    = GotExams (Data Exams) 
    | GotExamPeriod (Data ExamPeriod) 
    | ChangedName String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        GotExams exams ->
            ( { model | exams = exams }
            , Cmd.none
            )
        GotExamPeriod examPeriod ->
            case examPeriod of
                Api.Data.Success ep ->
                    ( { model | name = ep.name, examPeriod = examPeriod}
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | name = "" }
                    , Cmd.none)
                _->
                    ( { model | name = "" }
                    , Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
    { title = "Ispitni Rok"
    , body = 
    UI.layout 
    [ div [class "container"] [
        br[][],
        h2[class "ml"][text "Ispitni Rok"]
        ,div[class "mt-5"][
            div[class "form-group"][
                label[][text "Naziv Ispitnog Roka"],
                input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ],
                div[class "form-group"][
                label[][text "Pocetak Ispitnog Roka"],
                input[type_ "date", class "form-control"][]
                ],
                div[class "form-group"][
                label[][text "Pocetak Ispitnog Roka"],
                input[type_ "date", class "form-control"][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ],
            div[class "container mt-4"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Prijavljeni ispiti"]
                    ], viewExams model
                ]
            ]
    ] ]
    }
okButton : Model -> Html Msg
okButton model =
    if  model.name == "" || model.start == "" || model.end == ""  then
        div[][
            div[][text "Molimo unesite sve podatke!"],
            button[class "btn btn-success float-right" , disabled True][text "Save"]
        ]
    else
        div[][
            button[class "btn btn-success"][text "Save"]
        ]

viewExams : Model -> Html Msg
viewExams model =
  case model.exams of
    Api.Data.Success exams ->
        tbody []
        (List.map(\s -> tr [class "text-center"][
            td[] [text ((studentCardText s.student) ++ " " ++ (studentNameText s.student) ++ " " ++ (Utils.Time.formatDate s.date) )]  
            ]) exams)
    Api.Data.Loading ->
      text "Loading..."
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