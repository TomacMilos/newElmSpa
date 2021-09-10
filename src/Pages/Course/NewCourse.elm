module Pages.Course.NewCourse exposing (Model, Msg, page)

import Gen.Params.Course.NewCourse exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Html.Attributes exposing (class)
import Gen.Route as Route exposing (Route)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (href)
import Api.StudentApi exposing (Student)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html.Events exposing (onClick)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Bootstrap.Dropdown exposing (DropdownOption)
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (value, min)
import Html.Events exposing (onInput)
import Html.Attributes exposing (style)


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
      myDrop1State : Dropdown.State,
      name: String
    }


init : ( Model, Cmd Msg )
init =
    ( {myDrop1State = Dropdown.initialState, name= ""}, Cmd.none )



-- UPDATE


type Msg
    = MyDrop1Msg Dropdown.State | ChangedName String

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        ChangedName name ->
            ({model | name = name}, Cmd.none)


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
            h2[class "ml"][text "Kurs"]
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
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]

    else
        div[][
            button[class "btn btn-success mr-2"][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]]
        ]