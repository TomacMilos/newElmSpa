module Pages.Course.NewEnrollment.Id_ exposing (Model, Msg, page)

import Gen.Params.Course.NewEnrollment.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Html.Attributes exposing (class, type_, href, disabled, value, min, style)
import Gen.Route as Route exposing (Route)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Dropdown exposing (DropdownOption)
import Html.Events exposing (onClick, onInput)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Api.CourseApi
import Api.CourseApi exposing (Course)
import Utils.Route
import Request exposing (Request)
import Api.EnrollmentApi exposing (Enrollment)
import Browser.Navigation exposing (Key)

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
      myDrop1State : Dropdown.State,
      selected: String,
      students: Data Students,
      student: Student,
      courseId: String,
      course: Data Course,
      name: String,
      start: String,
      end: String,
      c: Course,
      e : Maybe (Data Enrollment)
    }

init : Params ->( Model, Cmd Msg )
init params =
    ( {
        courseId = params.id,myDrop1State = Dropdown.initialState, 
        selected= "Select Student", 
        students = Api.Data.Loading, 
        student = {id = 0, firstName = "Select", 
        lastName = "Student", cardNumber = ""}, 
        course = Api.Data.Loading, 
        name = "", 
        c = {id = 0, name = ""}, 
        start = "", 
        end="",
        e = Nothing
        },
        Cmd.batch
            [ 
            Api.StudentApi.get
                { 
                onResponse = GotStudents
                },
            Api.CourseApi.getById
                { 
                courseID = params.id,
                onResponse = GotCourse 
                }
            ]
    )

-- UPDATE

type Msg
    = GotStudents (Data Students) 
    | MyDrop1Msg Dropdown.State 
    | SelectStudent Student 
    | GotCourse (Data Course) 
    | ChangeStart String 
    | ChangeEnd String 
    | SubmittedForm 
    | GotEnrollment (Data Enrollment)

update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotStudents students ->
            ( { model | students = students }
            , Cmd.none
            )
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        SelectStudent student ->
            ({model | student = student}, Cmd.none)
        GotCourse course ->
            case course of
                Api.Data.Success c ->
                    ( { model | name = c.name, course = course, c = c }
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | name = "" }
                    , Cmd.none)
                _->
                    ( { model | name = "" }
                    , Cmd.none)
        ChangeStart start ->
            ( { model | start = start}
            , Cmd.none)
        ChangeEnd end ->
            ( { model | end = end }
            , Cmd.none)
        SubmittedForm ->
            ( model
            , Api.EnrollmentApi.create
                { enrollment =
                    { student = model.student
                    , course = model.c
                    , startDate = model.start
                    , endDate = model.end
                    }
                , onResponse = GotEnrollment
                }
            )
        GotEnrollment enrollment ->   
            ( { model | e = Just enrollment }
            , case enrollment of
                Api.Data.Success newEnrollment ->
                    Utils.Route.navigate key
                    (Route.Course__Id_ {id = model.courseId})
                _ ->
                    Cmd.none
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.myDrop1State MyDrop1Msg ]

-- VIEW

view : Model -> View Msg
view model =
      { title = "Enrollment"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text ("Dodavanje studenta na kurs " ++ model.name)]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Student"],
                        div []
                            [ Dropdown.dropdown
                                model.myDrop1State
                                { options = []
                                , toggleMsg = MyDrop1Msg
                                , toggleButton =
                                Dropdown.toggle [Button.outlineInfo ] [ text (model.student.cardNumber++" " ++model.student.firstName ++ " " ++model.student.lastName) ]
                                , items =  studentList model
                                }
                            ]
                ],
                div[class "form-group"][
                label[][text "Pocetak Kursa"],
                input[type_ "date", class "form-control", onInput ChangeStart][]
                ],
                div[class "form-group"][
                label[][text "Zavrsetak Kursa"],
                input[type_ "date", class "form-control", onInput ChangeEnd][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }
studentList : Model -> List (Bootstrap.Dropdown.DropdownItem Msg)
studentList model =
  case model.students of
    Api.Data.Success students ->
      (List.map(\s -> Dropdown.buttonItem [ onClick (SelectStudent s) ] [ text  (s.cardNumber++" "++ s.firstName ++ " " ++ s.lastName)]) students)
    Api.Data.Loading ->
      []
    _ ->
      []

okButton : Model -> Html Msg
okButton model =
    if model.student.id == 0 || model.start == "" || model.end == ""  then
        div[][
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref (Route.Course__Id_ { id = model.courseId}))] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2", onClick SubmittedForm][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]]
        ]