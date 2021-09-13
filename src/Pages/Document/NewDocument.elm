module Pages.Document.NewDocument exposing (Model, Msg, page)

import Gen.Params.Document.NewDocument exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Html.Attributes exposing (class, type_, href, disabled, value)
import Gen.Route as Route exposing (Route)
import Api.StudentApi exposing (Student)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Bootstrap.Dropdown exposing (DropdownOption)
import Html.Events exposing (onInput, onClick)
import Api.DocumentsApi
import Api.DocumentsApi exposing (Document)
import Utils.Route
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
      myDrop1State : Dropdown.State,
      selected: String,
      students: Data Students,
      student: Student,
      name: String,
      document: Maybe (Data Document)
    }

init : (Model, Cmd Msg )
init =
    ( { myDrop1State = Dropdown.initialState, selected= "Select Student", students = Api.Data.Loading, student = {id = 0, firstName = "Select", lastName = "Student", cardNumber = ""}, name = "",document = Nothing} -- initially closed
    , Cmd.batch
        [ get
            { 
              onResponse = GotStudents
            }
        ]
    )

-- UPDATE
type Msg
    = MyDrop1Msg Dropdown.State 
    | ChangedName String 
    | GotStudents (Data Students) 
    | SelectStudent Student 
    | SubmittedForm 
    | GotDocument (Data Document)

update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        GotStudents students ->
            ( { model | students = students }
            , Cmd.none
            )
        SelectStudent student ->
            ({model | student = student}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.DocumentsApi.create
                { document =
                    { naziv = model.name
                    , student = model.student
                    }
                , onResponse = GotDocument
                }
            )
        GotDocument document ->   
            ( { model | document = Just document }
            , case document of
                Api.Data.Success newDocument ->
                    Utils.Route.navigate req.key
                    (Route.Document__Documents)
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
      { title = "Novi Dokument"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Novi Dokument"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Naziv Dokumenta"],
                    input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ],
                div[class "form-group"][
                    label[][text "Student"],
                        div []
                            [ Dropdown.dropdown
                                model.myDrop1State
                                { options = []
                                , toggleMsg = MyDrop1Msg
                                , toggleButton =
                                Dropdown.toggle [Button.outlineInfo ] [ text (model.student.cardNumber++" " ++model.student.firstName ++ " " ++model.student.lastName) ]
                                , items =  studentList model
                                }
                            ]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }

studentList : Model -> List (Bootstrap.Dropdown.DropdownItem Msg)
studentList model =
  case model.students of
    Api.Data.Success students ->
      (List.map(\s -> Dropdown.buttonItem [ onClick (SelectStudent s) ] [ text  (s.cardNumber++" "++ s.firstName ++ " " ++ s.lastName)]) students)
    Api.Data.Loading ->
      []
    _ ->
      []

okButton : Model -> Html Msg
okButton model =
    if model.student.id == 0 || model.name == ""  then
        div[][
            button[class "btn btn-success mr-2" , disabled True][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]],
            div[][text "Molimo unesite sve podatke!"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2", onClick SubmittedForm][text "Ok"],
            a [ href (Route.toHref Route.Payment__Payments)] [
            button[class "btn btn-primary" ][text "Cancel"]]
        ]