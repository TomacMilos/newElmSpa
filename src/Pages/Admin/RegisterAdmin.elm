module Pages.Admin.RegisterAdmin exposing (Model, Msg, page)

import Gen.Params.Admin.RegisterAdmin exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Gen.Route as Route exposing (Route)
import Html.Attributes exposing (value, disabled, href, type_, class)
import Html.Events exposing (onInput, onClick)
import Api.Data exposing (Data)
import Api.AdminApi exposing (User)
import Request exposing (Request)
import Utils.Route

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
      ime: String,
      lozinka: String,
      relozinka: String,
      user: Maybe (Data User)

    }

init : ( Model, Cmd Msg )
init =
    ( 
        {
            ime = "", 
            lozinka = "", 
            relozinka = "", 
            user= Nothing
        }, 
        Cmd.none 
    )

-- UPDATE
type Msg
    =  ChangedIme String 
    | ChangedLozinka String 
    | ChangedRelozinka String 
    | SubmittedForm 
    | GotUser (Data User)

update :Request -> Msg -> Model -> ( Model, Cmd Msg ) 
update req msg model =
    case msg of
        ChangedIme ime ->
            ({model | ime = ime}, Cmd.none)
        ChangedLozinka lozinka ->
            ({model | lozinka = lozinka}, Cmd.none)
        ChangedRelozinka relozinka ->
            ({model | relozinka = relozinka}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.AdminApi.create
                { admin =
                    { username = model.ime
                    , password = model.lozinka
                    }
                , onResponse = GotUser
                }
            )
        GotUser user ->   
            ( { model | user = Just user }
            , case user of
                Api.Data.Success newUser ->
                    Utils.Route.navigate req.key
                    (Route.Student__Students)
                _ ->
                    Cmd.none
            )        

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
                    label[][text "Korisnicko Ime"],
                    input[type_ "text", class "form-control", value model.ime, onInput ChangedIme ][]
                ],
                div[class "form-group"][
                    label[][text "Lozinka"],
                    input[type_ "password", class "form-control", value model.lozinka, onInput ChangedLozinka ][],
                    passmess model
                ],
                div[class "form-group"][
                    label[][text "Ponovi Lozinku"],
                    input[type_ "password", class "form-control", value model.relozinka, onInput ChangedRelozinka ][],
                    passmess model
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }

okButton : Model -> Html Msg
okButton model =
    if  model.ime == "" || model.relozinka == "" || model.relozinka == "" || String.length model.lozinka < 8 || model.lozinka /= model.relozinka   then
        div[][
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref Route.Student__Students)] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2", onClick SubmittedForm][text "Ok"],
            a [ href (Route.toHref Route.Student__Students) ] [
            button[class "btn btn-primary" ][text "Cancel"]]
        ]

passmess : Model -> Html Msg
passmess model =
    if  String.length model.lozinka < 8  then
        p[][text "Lozinka mora da ima minimum 8 karaktera"]

    else if model.lozinka /= model.relozinka then
        p[][text "Lozinke se ne podudaraju"]
    else
        text ""