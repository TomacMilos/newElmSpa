module Pages.Teacher.Teachers exposing (Model, Msg, page)

import Gen.Params.Teacher.Teachers exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Api.Data exposing (Data)
import Api.TeacherApi exposing (..)
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
      teachers : Data Teachers
    }

init : (Model, Cmd Msg)
init =
    ( {
        teachers = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotTeachers
            }
        ]
    )

-- UPDATE
type Msg
  =  GotTeachers (Data Teachers) 
  |  DeletedTeacher (Data Int) 
  |  ClickedDeleteTeacher Teacher

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTeachers teachers ->
      ( { model | teachers = teachers }
        , Cmd.none
      )
    ClickedDeleteTeacher teacher ->
          ( model
          , delete
              { 
                teacherId = teacher.id
              , onResponse = DeletedTeacher 
              }
            )
    DeletedTeacher id ->
            let
                removeTeacher : Teachers -> Teachers
                removeTeacher =
                    List.filter (\teacher -> Api.Data.Success teacher.id /= id)
            in
            ( { model | teachers = Api.Data.map removeTeacher model.teachers }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Profesori"
    , body = UI.layout [ div [class "container"] [
            br[] [],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-users" ][],
              text " Profesori"]
            ,div[class "mt-5"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Ime"],
                        th[class "text-center"][text "Prezime"],
                        th[class "text-center"][text "Uloga"],
                        th[class "text-center"][
                            a [ href (Route.toHref Route.Teacher__NewTeacher)] [
                              button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                        ]
                    ], viewTeachers model
                ]
            ]
    ] ]
    }
viewTeachers : Model -> Html Msg
viewTeachers model =
  case model.teachers of
    Api.Data.Success teachers ->
        tbody []
        (List.map(\t -> tr [class "text-center"][
            td[class "cursor-pointer"][
            a[href (Route.toHref (Route.Teacher__Id_ { id = String.fromInt t.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text t.firstName]]]],     
            td[class "cursor-pointer"][
            a[href (Route.toHref (Route.Teacher__Id_ { id = String.fromInt t.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text t.lastName]]]],
            td[class "cursor-pointer"][
            a[href (Route.toHref (Route.Teacher__Id_ { id = String.fromInt t.id })), style "text-decoration" "none" , style "color" "black"] [
            div[style "display" "flex", style "justify-content" "center"][p[][text t.teacherRank]]]],
            td[][button[class "btn btn-danger", onClick (ClickedDeleteTeacher t)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]
            ]) teachers)
    Api.Data.Loading ->
      tr[][
      td[][],
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"