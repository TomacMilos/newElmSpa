module Pages.TeacherPages.TeacherPage exposing (Model, Msg, page)

import Gen.Params.TeacherPages.TeacherPage exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Api.Data exposing (Data)
import Api.TeacherApi exposing (Teacher)
import UI
import Api.CourseApi exposing (Courses)
import Html exposing (..)
import Html.Attributes exposing (type_, class, disabled, value, style)
import Gen.Route as Route exposing (Route)
import Html.Attributes exposing (href)


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
        teacher: Data Teacher,
        t: Teacher,
        courses: Data Courses

    }


init : ( Model, Cmd Msg )
init =
    ( {
        teacher = Api.Data.Loading, 
        t = {
            id = 0,
            firstName = "",
            lastName = "",
            teacherRank = ""
        },
        courses = Api.Data.Loading

    },
    Cmd.batch
        [ Api.CourseApi.getForTeacher
            { 
              teacherID = "1",
              onResponse = GotCourses
            },
            Api.TeacherApi.getById
            { 
              teacherID = "1",
              onResponse = GotTeacher
            }
        ] )



-- UPDATE


type Msg
    = GotTeacher (Data Teacher) 
    | GotCourses (Data Courses) 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTeacher teacher ->
            case teacher of
                Api.Data.Success t ->
                    ( { model | t = t}
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | t = {id = 0, firstName = "", lastName = "", teacherRank = ""} }
                    , Cmd.none)
                _->
                    ( { model | t = {id = 0, firstName = "", lastName = "", teacherRank = ""} }

                    , Cmd.none)
        GotCourses courses ->
            ( { model | courses = courses }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
      { title = "Profesor"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
                Html.i [ Html.Attributes.class "fas fa-user" ][],
                text "  Profesor"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Ime"],
                    input[type_ "text", class "form-control", value model.t.firstName, disabled True][]
                ],
                div[class "form-group"][
                    label[][text "Prezime"],
                    input[type_ "text", class "form-control", value model.t.lastName, disabled True][]
                ],
                div[class "form-group"][
                    label[][text "Uloga"],
                    input[type_ "text", class "form-control", value model.t.teacherRank, disabled True][]

                ]
            ],
            div[class "container mt-4"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Kursevi"]
                    ], viewCourses model
                ]
            ]
    ] ]
    }
viewCourses : Model -> Html Msg
viewCourses model =
  case model.courses of
    Api.Data.Success courses ->
        tbody []
        (List.map(\c -> tr [class "text-center cursor-pointer"][
            a [href (Route.toHref (Route.TeacherPages__Course__Id_{ id = String.fromInt c.id })), style "text-decoration" "none" , style "color" "black"] [
            td[class "text-center"] [text c.name]]
            ]) courses)
    Api.Data.Loading ->
      tr[][
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"