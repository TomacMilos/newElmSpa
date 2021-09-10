module Pages.Admin.RegisterAdmin exposing (Model, Msg, page)

import Gen.Params.Admin.RegisterAdmin exposing (Params)
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
      ime: String,
      lozinka: String,
      relozinka: String
    }

init : ( Model, Cmd Msg )
init =
    ( {ime = "", lozinka = "", relozinka = ""}, Cmd.none )





-- UPDATE


type Msg
    =  ChangedIme String | ChangedLozinka String | ChangedRelozinka String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedIme ime ->
            ({model | ime = ime}, Cmd.none)
        ChangedLozinka lozinka ->
            ({model | lozinka = lozinka}, Cmd.none)
        ChangedRelozinka relozinka ->
            ({model | relozinka = relozinka}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> View Msg
view model =
      { title = "Dodavanje Admina"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Dodaj novog admina"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Ime Studenta"],
                    input[type_ "text", class "form-control", value model.ime, onInput ChangedIme ][]
                ],
                div[class "form-group"][
                    label[][text "Lozinka"],
                    input[type_ "text", class "form-control", value model.lozinka, onInput ChangedLozinka ][]
                ],
                div[class "form-group"][
                    label[][text "Ponovi Lozinku"],
                    input[type_ "text", class "form-control", value model.relozinka, onInput ChangedRelozinka ][]
                ]

            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }

okButton : Model -> Html Msg
okButton model =
    if  model.ime == "" || model.relozinka == "" || model.relozinka == ""  then
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