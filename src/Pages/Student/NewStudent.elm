module Pages.Student.NewStudent exposing (Model, Msg, page)

import Gen.Params.Student.NewStudent exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Gen.Route as Route exposing (Route)
import Api.StudentApi exposing (Student)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (disabled, value, href, type_, class)
import Request exposing (Request)
import Utils.Route
import Api.Data exposing (Data)

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
      prezime: String,
      lozinka: String,
      relozinka: String,
      student: Maybe (Data Student)
    }
init : ( Model, Cmd Msg )
init =
    ( 
        {
            ime = "",
            prezime= "", 
            lozinka = "", 
            relozinka = "", 
            student = Nothing
        }
        , Cmd.none )

-- UPDATE
type Msg
    =  ChangedIme String 
    | ChangedPrezime String 
    | ChangedLozinka String 
    | ChangedRelozinka String 
    | SubmittedForm 
    | GotStudent (Data Student)

update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        ChangedIme ime ->
            ({model | ime = ime}, Cmd.none)
        ChangedPrezime prezime ->
            ({model | prezime = prezime}, Cmd.none)
        ChangedLozinka lozinka ->
            ({model | lozinka = lozinka}, Cmd.none)
        ChangedRelozinka relozinka ->
            ({model | relozinka = relozinka}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.StudentApi.create
                { student =
                    { firstName = model.ime
                    , lastName = model.prezime
                    , password = model.lozinka
                    }
                , onResponse = GotStudent
                }
            )
        GotStudent student ->   
            ( { model | student = Just student }
            , case student of
                Api.Data.Success newStudent ->
                    Utils.Route.navigate req.key
                    (Route.Student__Students)
                _ ->
                    Cmd.none
            )        

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Dodavanje Studenta"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Dodavanje Studenta"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Ime Studenta"],
                    input[type_ "text", class "form-control", value model.ime, onInput ChangedIme ][]
                ],
                div[class "form-group"][
                    label[][text "Prezime Studenta"],
                    input[type_ "text", class "form-control", value model.prezime, onInput ChangedPrezime ][]
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
    if  model.ime == "" || model.prezime == "" || model.lozinka == "" || model.relozinka == "" || String.length model.lozinka < 8 || model.lozinka /= model.relozinka  then
        div[][
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref Route.Student__Students)] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2", onClick SubmittedForm][text "Ok"],
            a [ href (Route.toHref Route.Student__Students)] [
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