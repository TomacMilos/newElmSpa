module Pages.Teacher.NewTeacher exposing (Model, Msg, page)

import Gen.Params.Teacher.NewTeacher exposing (Params)
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
import Api.Data exposing (Data)
import Api.TeacherApi exposing (Teacher)
import Utils.Route
import Api.Data exposing (Data)
import Request exposing (Request)



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
      uloga: String,
      korisnickoIme: String,
      lozinka: String,
      relozinka: String,
      myDrop1State : Dropdown.State,
      teacher: Maybe (Data Teacher)

    }


init : ( Model, Cmd Msg )
init =
    ( { myDrop1State = Dropdown.initialState, ime = "",prezime= "", lozinka = "", relozinka = "", uloga ="Izaberi Ulogu", korisnickoIme = "", teacher = Nothing}, Cmd.none )



-- UPDATE


type Msg
    =  ChangedIme String | ChangedPrezime String | ChangedLozinka String | ChangedRelozinka String | ChangedUloga String | ChangedKorisnickoIme String | MyDrop1Msg Dropdown.State | SubmittedForm | GotTeacher (Data Teacher)


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )   
        ChangedIme ime ->
            ({model | ime = ime}, Cmd.none)
        ChangedPrezime prezime ->
            ({model | prezime = prezime}, Cmd.none)
        ChangedLozinka lozinka ->
            ({model | lozinka = lozinka}, Cmd.none)
        ChangedRelozinka relozinka ->
            ({model | relozinka = relozinka}, Cmd.none)
        ChangedUloga uloga ->
            ({model | uloga = uloga}, Cmd.none)
        ChangedKorisnickoIme korisnickoIme ->
            ({model | korisnickoIme = korisnickoIme}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.TeacherApi.create
                { teacher =
                    { firstName = model.ime
                    , lastName = model.prezime
                    , password = model.lozinka
                    , teacherRank = model.uloga
                    , userName = model.korisnickoIme
                    }
                , onResponse = GotTeacher
                }
            )
        GotTeacher teacher ->   
            ( { model | teacher = Just teacher }
            , case teacher of
                Api.Data.Success newTeacher ->
                    Utils.Route.navigate req.key
                    (Route.Teacher__Teachers)
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
      { title = "Dodavanje Profesora"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Dodavanje Profesora"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Ime Profesora"],
                    input[type_ "text", class "form-control", value model.ime, onInput ChangedIme ][]
                ],
                div[class "form-group"][
                    label[][text "Prezime Profesora"],
                    input[type_ "text", class "form-control", value model.prezime, onInput ChangedPrezime ][]
                ],
                div[class "form-group"][
                    label[][text "Uloga"],
                    div []
                    [ Dropdown.dropdown
                        model.myDrop1State
                        { options = []
                        , toggleMsg = MyDrop1Msg
                        , toggleButton =
                        Dropdown.toggle [Button.outlineInfo ] [ text model.uloga]
                        , items =
                        [ Dropdown.buttonItem [ onClick (ChangedUloga "Profesor")  ] [ text "Profesor" ]
                        , Dropdown.buttonItem [ onClick (ChangedUloga "Asistent") ] [ text "Asistent" ]
                        , Dropdown.buttonItem [ onClick (ChangedUloga "Demonstrator") ] [ text "Demonstrator" ]

                        ]
                        }
                    ]
                ],
                div[class "form-group"][
                    label[][text "Korisicko Ime"],
                    input[type_ "text", class "form-control", value model.korisnickoIme, onInput ChangedKorisnickoIme ][]
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
    if  model.ime == "" || model.prezime == "" || model.lozinka == "" || model.relozinka == "" || model.uloga == "Izaberi Ulogu" || model.korisnickoIme == "" || String.length model.lozinka < 8 || model.lozinka /= model.relozinka then
        div[][
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]

    else
        div[][
            button[class "btn btn-success mr-2" , onClick SubmittedForm][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
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