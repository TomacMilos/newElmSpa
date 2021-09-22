module Pages.StudentsPages.ExamRegistration.Id_ exposing (Model, Msg, page)

import Gen.Params.StudentsPages.ExamRegistration.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Html exposing (..)
import Html.Attributes exposing (class)
import Api.ExamApi exposing (Exams)
import Utils.Route
import Api.Data exposing (Data)
import Utils.Time
import Api.StudentApi exposing (Student)
import Gen.Route as Route exposing (Route)
import Api.ExamApi exposing (Exam)
import Browser.Navigation exposing (Key)
import Html.Events exposing (onClick)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update req.key
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
      exams : Data Exams,
      student: Data Student,
      s : Student,
      ex: Maybe (Data Exam)
      
    }


init : Params -> ( Model, Cmd Msg )
init params =
    ( {
        exams = Api.Data.Loading,
        student = Api.Data.Loading,
        s  = {
            id = 0,
            firstName = "",
            lastName = "",
            cardNumber = ""
        },
        ex = Nothing


    }
    , Cmd.batch
        [ 
            Api.ExamApi.getExamsForExamPeriod
            { 
              studentId = "1",
              examPeriodId = params.id,
              onResponse = GotExams
            },
            Api.StudentApi.getById
                { 
                studentID = "1",
                onResponse = GotStudent
                }
        ]
    )



-- UPDATE


type Msg
  =  GotExams (Data Exams) 
    | GotStudent (Data Student)
    | RegisterExams (Data Exam) 
    | ClickedRegisterExams Exam

update :Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotExams exams ->
            ( { model | exams = exams }
                , Cmd.none
            )
        GotStudent student ->
            ( { model | student = student , s = getStudent student}
                , Cmd.none
            )
        ClickedRegisterExams exam ->
            ( model
            , Api.ExamApi.registerExam
                { 
                    examDTO = {
                        course = exam.course,
                        date = exam.date,
                        examPeriod = exam.examPeriod,
                        examPoints = exam.examPoints,
                        id = exam.id,
                        labPoints = exam.labPoints,
                        student = {
                            id = model.s.id,
                            firstName = model.s.firstName,
                            lastName = model.s.lastName,
                            cardNumber = model.s.cardNumber
                        }
                    }
                , onResponse = RegisterExams 
                }
                )
        RegisterExams exam ->
            ( { model | ex = Just exam }
            , case exam of
                Api.Data.Success newExam ->
                    Utils.Route.navigate key
                    (Route.StudentsPages__StudentPage)
                _ ->
                    Cmd.none
            )
getStudent: Data Student -> Student
getStudent student = 
    case student of
        Api.Data.Success s->
            s
        Api.Data.Loading ->
            {
                id = 0,
                firstName = "",
                lastName = "",
                cardNumber = ""
            }
        _ ->
            {
                id = 0,
                firstName = "",
                lastName = "",
                cardNumber = ""
            }
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
      { title = "Prijava Ispita"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-check-square" ][]
              ,text " Prijava Ispita"]
            ,div[class "mt-5"][
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Predmet"],
                        th[class "text-center"][text "Vreme Odrzavanja"],
                        th[class "text-center"][text "Prijava Ispita"]
                    ]
                    , viewExams model
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
            td[] [text e.course.name],
            td[] [text (Utils.Time.formatDate e.date)],
            td[][button[class "btn btn-primary" , onClick (ClickedRegisterExams e) ][Html.i [ Html.Attributes.class "fas fa-check-square" ][]]]
            ]) exams)
    Api.Data.Loading ->
      tr[][
      td[][],
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"