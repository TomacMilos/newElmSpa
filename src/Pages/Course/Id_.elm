module Pages.Course.Id_ exposing (Model, Msg, page)

import Gen.Params.Course.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Html.Attributes exposing (class, href, type_, disabled, value)
import Gen.Route as Route exposing (Route)
import Html.Events exposing (onClick, onInput)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Api.CourseApi
import Utils.Route
import Utils.Time
import Request exposing (Request)
import Api.CourseApi exposing (Course)
import Api.EnrollmentApi exposing (Enrollments, Enrollment)
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
      course: Data Course,
      name: String,
      id: String,
      enrollments: Data Enrollments,
      c: Maybe (Data Course)
    }


init : Params -> ( Model, Cmd Msg )
init params=
    ( 
        {
            id = params.id, 
            enrollments = Api.Data.Loading, 
            course = Api.Data.Loading, 
            name = "", c = Nothing
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
                    }
            ]
    )

-- UPDATE
type Msg
    = ChangedName String 
    | GotEnrollment (Data Enrollments) 
    | GotCourse (Data Course)
    | DeletedEnrollment (Data Int) 
    | ClickedDeleteEnrollment Enrollment 
    | SubmittedForm 
    | Saved (Data Course)

update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        GotEnrollment enrollments ->
            ( { model | enrollments = enrollments }
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
        ClickedDeleteEnrollment enrollment ->
              ( model
            , Api.CourseApi.deleteEnrollment
               { 
                enrollmentID = enrollment.id
                , onResponse = DeletedEnrollment 
                }
                )
        DeletedEnrollment id ->
            let
                removeEnrollment : Enrollments -> Enrollments
                removeEnrollment =
                    List.filter (\enrollment -> Api.Data.Success enrollment.id /= id)
            in
            ( { model | enrollments = Api.Data.map removeEnrollment model.enrollments }
            , Cmd.none
            )
        SubmittedForm ->
            ( model
            , Api.CourseApi.update
                { courseDTO =
                    { 
                      id = valueInt (String.toInt model.id),
                      name = model.name
                    }
                , onResponse = Saved
                }
            )
        Saved course ->
            ( { model | c = Just course }
            , case course of
                Api.Data.Success newCourse ->
                    Utils.Route.navigate key
                    (Route.Course__Courses)
                _ ->
                    Cmd.none
            )
valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 

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
                    input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Studenti"]
                ],
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ime i Prezime"],
                        th[class "text-center"][text "Broj Indeksa"],
                        th[class "text-center"][text "Pocetak Kursa"],
                        th[class "text-center"][text "Zavrsetak Kursa"],
                        th[class "text-center"][
                          a [ href (Route.toHref (Route.Course__NewEnrollment__Id_ { id = model.id }))] [
                            button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                        ]
                    ], viewEnrollments model
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
            td[] [text (Utils.Time.formatDate s.endDate)],

            td[][button[class "btn btn-danger", onClick (ClickedDeleteEnrollment s)][
              Html.i [ Html.Attributes.class "fas fa-trash" ][] ]]
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


okButton : Model -> Html Msg
okButton model =
    if  model.name == ""  then
        div[][
            div[][text "Molimo unesite sve podatke!"],
            button[class "btn btn-success float-right" , disabled True][Html.i [ Html.Attributes.class "fas fa-save" ][]]
        ]

    else
        div[][
            button[class "btn btn-success", onClick SubmittedForm][Html.i [ Html.Attributes.class "fas fa-save" ][]]
        ]