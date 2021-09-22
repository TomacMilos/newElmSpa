module Pages.TeacherPages.Course.Id_ exposing (Model, Msg, page)

import Gen.Params.TeacherPages.Course.Id_ exposing (Params)
import Page
import Gen.Route as Route exposing (Route)
import Request
import Shared
import View exposing (View)
import Api.Data exposing (Data)
import Api.CourseApi exposing (Course)
import Api.EnrollmentApi exposing (Enrollments)
import Browser.Navigation exposing (Key)
import Html.Attributes exposing (class, href, type_, disabled, value)
import Html exposing (..)
import Utils.Time
import UI
import Api.ExamApi exposing (Exams)
import Api.StudentApi exposing (Student)
import Html.Attributes exposing (style)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update =  update req.key
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
      course: Data Course,
      name: String,
      id: String,
      enrollments: Data Enrollments,
      c: Maybe (Data Course),
      exams: Data Exams
    }


init : Params -> ( Model, Cmd Msg )
init params=
    ( 
        {
            id = params.id, 
            enrollments = Api.Data.Loading, 
            course = Api.Data.Loading, 
            name = "", c = Nothing,
            exams = Api.Data.Loading

        },
        Cmd.batch
            [ 
                Api.EnrollmentApi.get
                    { 
                    courseID = params.id,
                    onResponse = GotEnrollment
                    },
                Api.CourseApi.getById
                    { 
                    courseID = params.id,
                    onResponse = GotCourse 
                    },
                Api.ExamApi.examsForTeacher
                    {
                        courseID = params.id,
                        onResponse = GotExams
                    }
            ]
    )


-- UPDATE

type Msg
    = 
      GotEnrollment (Data Enrollments) 
    | GotCourse (Data Course)
    | GotExams (Data Exams)



update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotEnrollment enrollments ->
            ( { model | enrollments = enrollments }
            , Cmd.none
            )
        GotExams exams ->
            ( { model | exams = exams }
            , Cmd.none
            )
        GotCourse course ->
            case course of
                Api.Data.Success c ->
                    ( { model | name = c.name, course = course }
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
      { title = "Kurs"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Kurs"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Naziv Kursa"],
                    input[type_ "text", class "form-control", disabled True, value model.name][]
                ]
            ],
            div[class "container mt-5"][
                div[][
                    h3[][text "Studenti"]
                ],
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ime i Prezime"],
                        th[class "text-center"][text "Broj Indeksa"],
                        th[class "text-center"][text "Pocetak Kursa"],
                        th[class "text-center"][text "Zavrsetak Kursa"]
                    ], viewEnrollments model
                ]
            ],
            div[class "container mt-5"][
                div[][
                    h3[][text "Ispiti"]
                ],
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ispitni Rok"],
                        th[class "text-center"][text "Naziv Predmeta"],
                        th[class "text-center"][text "Datum"],
                        th[class "text-center"][text "Vreme Ispita"],
                        th[class "text-center"][text "Ime i Prezime"],
                        th[class "text-center"][text "Broj Indeks"],
                        th[class "text-center"][text "Upis Ocene"]
                    ], viewExams model
                ]
            ]
    ] ]
    }

viewEnrollments : Model -> Html Msg
viewEnrollments model =
  case model.enrollments of
    Api.Data.Success enrollments ->
        tbody []
        (List.map(\s -> tr [class "text-center"][
            td[] [text (s.student.firstName ++ " " ++ s.student.lastName)],  
            td[] [text s.student.cardNumber],
            td[] [text (Utils.Time.formatDate s.startDate)],
            td[] [text (Utils.Time.formatDate s.endDate)]
            ]) enrollments)
    Api.Data.Loading ->
        tr[][
            td[][],
            td[][],
            div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin" ][]
        ]
        ]
    _ ->
      text "Fail"

viewExams : Model -> Html Msg
viewExams model =
  case model.exams of
    Api.Data.Success exams ->
        tbody []
        (List.map(\e -> tr [class "text-center"][
            td[] [text e.examPeriod.name],
            td[] [text e.course.name],  
            td[] [text (Utils.Time.formatDate e.date)],
            td[] [text (Utils.Time.formatTime e.date)],
            td[] [text (studentNameText e.student)],
            td[] [text (studentCardText e.student)],
            a [href (Route.toHref (Route.TeacherPages__UpisOcene__Id_ { id = String.fromInt e.id }))] [
            td[] [button[class "btn btn-success mr-2"][text "Upis Ocene"]]]
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