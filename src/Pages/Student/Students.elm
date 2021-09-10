module Pages.Student.Students exposing (Model, Msg, page)

import Gen.Params.Student.Students exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Html exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Attributes exposing (href)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)

import Html.Events exposing (onClick)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { students : Data Students
    }

init : (Model, Cmd Msg)
init =
    ( {
        students = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotStudents
            }
        ]
    )

-- UPDATE


type Msg
  = GotStudents (Data Students) |  DeletedStudent (Data Int) |  ClickedDeleteStudent Student
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotStudents students ->
      ( { model | students = students }
        , Cmd.none
      )
    ClickedDeleteStudent student ->
          ( model
          , delete
              { 
                studentId = student.id
              , onResponse = DeletedStudent 
              }
            )
    DeletedStudent id ->
            let
                removeStudent : Students -> Students
                removeStudent =
                    List.filter (\student -> Api.Data.Success student.id /= id)
            in
            ( { model | students = Api.Data.map removeStudent model.students }
            , Cmd.none
            )



-- VIEW


view : Model -> View Msg
view model =
      { title = "Studenti"
    , body = UI.layout [ div [class "container"] [
            br[] [],
            h2[class "ml"][text "Studenti"]
            ,div[class "mt-5"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Index"],
                        th[class "text-center"][text "Ime"],
                        th[class "text-center"][text "Prezime"],
                        th[class "text-center"]
                        [
                          a [ href (Route.toHref Route.Student__NewStudent)] [
                          button[class "btn btn-success"][text "Dodaj"]]
                          ]
                    ],viewStudents model
                ]
            ]
    ] ]
    }

viewStudents : Model -> Html Msg
viewStudents model =
  case model.students of
    Api.Data.Success students ->
        tbody []
        (List.map(\s -> tr [class "text-center"][
            td[class "cursor-pointer"] [text s.cardNumber],  
            td[class "cursor-pointer"] [text s.firstName],
            td[class "cursor-pointer"] [text s.lastName],
            td[][button[class "btn btn-danger", onClick (ClickedDeleteStudent s)][
              text "Obrisi" ]]
            
            ]) students)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none