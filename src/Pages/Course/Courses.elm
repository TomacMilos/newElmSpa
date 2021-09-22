module Pages.Course.Courses exposing (Model, Msg, page)

import Gen.Params.Course.Courses exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Api.Data exposing (Data)
import Api.CourseApi exposing (..)
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
      courses : Data (List Course)
    }

init : (Model, Cmd Msg)
init =
    ( {
        courses = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotCourses
            }
        ]
    )

-- UPDATE

type Msg
  = GotCourses (Data Courses) 
  |  DeletedCourse (Data Int) 
  |  ClickedDeleteCourse Course

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotCourses courses ->
      ( { model | courses = courses }
        , Cmd.none
      )
    ClickedDeleteCourse course ->
          ( model
          , delete
              { 
                courseId = course.id
              , onResponse = DeletedCourse 
              }
            )
    DeletedCourse id ->
            let
                removeCourse : List Course -> List Course
                removeCourse =
                    List.filter (\course -> Api.Data.Success course.id /= id)
            in
            ( { model | courses = Api.Data.map removeCourse model.courses }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Kursevi"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-chalkboard-teacher" ][],
              text " Kursevi"]
            ,div[class "mt-5"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv"],
                        th[class "text-center"][
                          a [ href (Route.toHref Route.Course__NewCourse)] [
                            button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                        ]
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
        (List.map(\c -> tr [class "text-center"][
            a [href (Route.toHref (Route.Course__Id_ { id = String.fromInt c.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][
            td[class "cursor-pointer text-center"] [text c.name]]],
            td[][button[class "btn btn-danger", onClick (ClickedDeleteCourse c)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]
            ]) courses)
    Api.Data.Loading ->
      tr[][
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"