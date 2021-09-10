module Pages.Payment.NewPayment exposing (Model, Msg, page)

import Gen.Params.Payment.NewPayment exposing (Params)
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
import Api.PaymentApi
import Api.PaymentApi exposing (Payment)
import Html.Events exposing (onSubmit)
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
      myDrop1State : Dropdown.State,
      selected: String,
      students: Data Students,
      student: Student,
      name: String,
      value: Maybe Int,
      payment: Maybe (Data Payment)
    }

init : (Model, Cmd Msg )
init =
    ( { myDrop1State = Dropdown.initialState, selected= "Select Student", students = Api.Data.Loading, student = {id = 0, firstName = "Select", lastName = "Student", cardNumber = ""}, value = Just 0, name = ""
    , payment = Nothing} -- initially closed
    , Cmd.batch
        [ get
            { 
              onResponse = GotStudents
            }
        ]
    )

-- UPDATE


type Msg
    = MyDrop1Msg Dropdown.State | ChangedName String | ChangedValue String | GotStudents (Data Students) | SelectStudent Student | SubmittedForm | GotPayment (Data Payment)


update :Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        ChangedValue value ->
            ({model | value = (String.toInt value)}, Cmd.none)
        GotStudents students ->
            ( { model | students = students }
            , Cmd.none
            )
        SelectStudent student ->
            ({model | student = student}, Cmd.none)
        
        SubmittedForm ->
            ( model
            , Api.PaymentApi.create
                { payment =
                    { svrhaUplate = model.name
                    , vrednostUplate = (valueInt model.value)
                    , date = ""
                    , student = model.student
                    }
                , onResponse = GotPayment
                }
            )
        GotPayment payment ->   
            ( { model | payment = Just payment }
            , case payment of
                Api.Data.Success newPayment ->
                    Utils.Route.navigate req.key
                    (Route.Payment__Payments)
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
      { title = "Nova Uplata"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Nova Uplata"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Svrha Uplate"],
                    input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ],
                div[class "form-group"][
                    label[][text "Vrednost Uplate"],
                    input[type_ "number", Html.Attributes.min "0" ,class "form-control", value (String.fromInt (valueInt model.value)) , onInput ChangedValue][]
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
valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 

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
    if model.student.id == 0 || model.name == "" || model.value == Just 0  then
        div[][
            button[class "btn btn-success mr-2", disabled True][text "Ok"],
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