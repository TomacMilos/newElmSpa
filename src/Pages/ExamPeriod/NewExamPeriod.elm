module Pages.ExamPeriod.NewExamPeriod exposing (Model, Msg, page)

import Gen.Params.ExamPeriod.NewExamPeriod exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Shared
import Html exposing (..)
import Gen.Route as Route exposing (Route)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Bootstrap.Dropdown exposing (DropdownOption)
import Html.Attributes exposing (disabled, value, type_, href, class)
import Html.Events exposing (onInput, onClick)
import Api.ExamPeriodApi exposing (ExamPeriod)
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
      name: String,
      start: String,
      end: String,
      examPeriod: Maybe (Data ExamPeriod)

    }

init : ( Model, Cmd Msg )
init =
    ( {
        name = "" ,
        start = "", 
        end ="", 
        examPeriod = Nothing
        },
         Cmd.none )

-- UPDATE
type Msg
    = ChangedName String 
    | ChangedStart String 
    | ChangedEnd String
    | SubmittedForm 
    | GotExamPeriod (Data ExamPeriod)

update :Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        ChangedName name ->
            ({model | name = name}, Cmd.none)
        ChangedStart start ->
            ({model | start = start}, Cmd.none)
        ChangedEnd end ->
            ({model | end = end}, Cmd.none)
        SubmittedForm ->
            ( model
            , Api.ExamPeriodApi.create
                { examPeriod =
                    { name = model.name,
                      startDate = model.start,
                      endDate = model.end
                    }
                , onResponse = GotExamPeriod
                }
            )
        GotExamPeriod examPeriod ->   
            ( { model | examPeriod = Just examPeriod }
            , case examPeriod of
                Api.Data.Success newCourse ->
                    Utils.Route.navigate req.key
                    (Route.ExamPeriod__ExamPeriods)
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
    { title = "Novi Ispitni Rok"
    , body = 
    UI.layout 
    [ div [class "container"] [
        br[][],
        h2[class "ml"][text "Novi Ispitni Rok"]
        ,div[class "mt-5"][
            div[class "form-group"][
                label[][text "Naziv Ispitnog Roka"],
                input[type_ "text", class "form-control", value model.name, onInput ChangedName ][]
                ],
                div[class "form-group"][
                label[][text "Pocetak Ispitnog Roka"],
                input[type_ "date", class "form-control", value model.start, onInput ChangedStart][]
                ],
                div[class "form-group"][
                label[][text "Pocetak Ispitnog Roka"],
                input[type_ "date", class "form-control", value model.end, onInput ChangedEnd ][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }

okButton : Model -> Html Msg
okButton model =
    if model.name == ""  then
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
