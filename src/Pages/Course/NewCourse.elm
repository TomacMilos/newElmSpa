module Pages.Course.NewCourse exposing (Model, Msg, page)

import Gen.Params.Course.NewCourse exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Gen.Route as Route exposing (Route)
import Bootstrap.Dropdown as Dropdown
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Bootstrap.Dropdown exposing (DropdownOption)
import Html.Attributes exposing (disabled, value, href, type_, class)
import Html.Events exposing (onInput, onClick)
import Api.CourseApi
import Utils.Route
import Request exposing (Request)
import Api.CourseApi exposing (Course)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update req
        , view = view
        , subscriptions = subscriptions
        }

-- INIT
type alias Model =
    { 
      myDrop1State : Dropdown.State,
      name: String,
      course: Maybe (Data Course)

    }

init : ( Model, Cmd Msg )
init =
    ( {myDrop1State = Dropdown.initialState, name= "", course = Nothing}, Cmd.none )

-- UPDATE

type Msg
    = MyDrop1Msg Dropdown.State 
    | ChangedName String 
    | SubmittedForm 
    | GotCourse (Data Course)

update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.CourseApi.create
                { course =
                    { name = model.name
                    }
                , onResponse = GotCourse
                }
            )
        GotCourse course ->   
            ( { model | course = Just course }
            , case course of
                Api.Data.Success newCourse ->
                    Utils.Route.navigate req.key
                    (Route.Course__Courses)
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
      { title = "Novi Kurs"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-chalkboard-teacher" ][],
                text " Kurs"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Naziv Kursa"],
                    input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }

okButton : Model -> Html Msg
okButton model =
    if  model.name == ""  then
        div[][
            button[class "btn btn-success mr-2" , disabled True][Html.i [ Html.Attributes.class "fas fa-save" ][]],
            a [ href (Route.toHref Route.Course__Courses)] [
            button[class "btn btn-primary" ][Html.i [ Html.Attributes.class "fas fa-times" ][]]],
            div[][text "Molimo unesite sve podatke!"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2", onClick SubmittedForm][Html.i [ Html.Attributes.class "fas fa-save" ][]],
            a [ href (Route.toHref Route.Course__Courses)] [
            button[class "btn btn-primary" ][Html.i [ Html.Attributes.class "fas fa-times" ][]]]
        ]