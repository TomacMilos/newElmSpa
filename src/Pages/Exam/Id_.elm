module Pages.Exam.Id_ exposing (Model, Msg, page)

import Gen.Params.Exam.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Html.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Api.ExamApi exposing (Exam)
import Api.Data exposing (Data)
import Utils.Time
import Api.StudentApi exposing (Student)
import Html.Events exposing (onClick)
import Browser.Navigation exposing (Key)
import Gen.Route as Route exposing (Route)
import Utils.Route

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update req.key
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {
        datum: String,
        student: String,
        predmet: String,
        examPoint: String,
        labPoint: String,
        ispitniRok: String,
        exam: Data Exam,
        e: Maybe (Data Exam),
        id: String
    }


init :Params -> ( Model, Cmd Msg )
init params=
    ( {
        datum = ""
    , student = ""
    , predmet = ""
    , examPoint = "0"
    , labPoint = "0"
    , ispitniRok = ""
    , e = Nothing
    , id = "0"
    , exam = Api.Data.Loading
    }, 
    Cmd.batch [
        Api.ExamApi.getById
            { 
              examID = params.id,
              onResponse = GotExam
            }
    ] )



-- UPDATE


type Msg
    = ChangeExamPoint String
    | ChangeLabPoint String
    | SubmittedForm Exam 
    | Saved (Data Exam) 
    | GotExam (Data Exam) 

update :Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        ChangeExamPoint value ->
            ({model | examPoint = (value)}, Cmd.none)
        ChangeLabPoint value ->
            ({model | labPoint = (value)}, Cmd.none)
        GotExam exam ->
            case exam of
                Api.Data.Success e ->
                    ( { model | id = String.fromInt e.course.id, exam = exam , datum = (Utils.Time.formatDate e.date) ++" " ++ (Utils.Time.formatTime e.date), student = studentNameText e.student, predmet = e.course.name, labPoint = String.fromInt e.labPoints, examPoint = String.fromInt e.examPoints, ispitniRok = e.examPeriod.name}
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | id = "0", datum = "", student = "", predmet = "", labPoint = "0", examPoint = "0", ispitniRok = ""}
                    , Cmd.none)
                _->
                    ( { model | id = "0", datum = "", student = "", predmet = "", labPoint = "0", examPoint = "0", ispitniRok = ""}
                    , Cmd.none)
        SubmittedForm exam ->
            ( model
            , Api.ExamApi.update
                { examDTO =
                    { 
                      id = exam.id,
                      course = exam.course,
                      date = exam.date,
                      examPeriod = exam.examPeriod,
                      examPoints = valueInt (String.toInt model.examPoint),
                      labPoints = valueInt (String.toInt model.labPoint),
                      student = setStudent exam.student
                    }
                , onResponse = Saved
                }
            )
        Saved exam ->
            ( { model | e = Just exam }
            , case exam of
                Api.Data.Success newStudent ->
                    Utils.Route.navigate key
                    (Route.Student__Id_ { id = model.id})
                _ ->
                    Cmd.none
            )
-- SUBSCRIPTIONS

setStudent: Maybe Student -> Student
setStudent student =
  case student of
    Nothing -> {id = 0, firstName = "" , lastName = "", cardNumber = ""}
    Just value -> {id = value.id, firstName = value.firstName , lastName = value.lastName, cardNumber = value.cardNumber}

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

studentNameText: Maybe Student -> String
studentNameText student =
  case student of
    Nothing -> ""
    Just value -> value.firstName ++ " " ++ value.lastName

studentCardText: Maybe Student -> String
studentCardText student =
  case student of
    Nothing -> ""
    Just value -> value.cardNumber

-- VIEW


view : Model -> View Msg
view model =
      { title = "Ispit"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
                Html.i [ Html.Attributes.class "fas fa-scroll" ][],
                text " Ispit"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][
                        text "Datum"],
                    input[type_ "text", class "form-control", disabled True , value model.datum][]
                ],
                div[class "form-group"][
                    label[][
                        text "Student"],
                    input[type_ "text", class "form-control", disabled True, value model.student][],
                 div[class "form-group"][
                    label[][
                        text "Predmet"],
                    input[type_ "text", class "form-control", disabled True, value model.predmet][]
                ]          ,
                div[class "form-group"][
                    label[][
                        text "Bodovi sa ispita"],
                    input[type_ "number",Html.Attributes.min "0", class "form-control", onInput ChangeExamPoint, value (String.fromInt (valueInt (String.toInt model.examPoint)))][]
                    ,message model
                ],
                div[class "form-group"][
                    label[][
                        text "Bodovi sa predispitnih obaveza"],
                    input[type_ "number",Html.Attributes.min "0", class "form-control", onInput ChangeLabPoint, value (String.fromInt (valueInt (String.toInt model.labPoint)))][]
                    ,message model
                ],
                div[class "form-group"][
                    label[][
                        text "Ispitni rok"],
                    input[type_ "text", class "form-control", disabled True, value model.ispitniRok][]
                ]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ]
    ] ]
    }
okButton : Model -> Html Msg
okButton model =
 case model.exam of
    Api.Data.Success exam ->
        if valueInt (String.toInt model.labPoint) + valueInt (String.toInt model.examPoint) > 100  then
            div[][
                button[class "btn btn-success mr-2" , disabled True][Html.i [ Html.Attributes.class "fas fa-save" ][]]
            ]
        else
            div[][
                button[class "btn btn-success mr-2", onClick (SubmittedForm exam)][Html.i [ Html.Attributes.class "fas fa-save"][]]
            ]
    Api.Data.Loading ->
      text ""
    _ ->
      text "Fail"
message : Model -> Html Msg
message model =
    if valueInt (String.toInt model.labPoint) + valueInt (String.toInt model.examPoint) > 100  then
        div[][text "Zbir bodova ne moze biti veci od 100!"]
    else
        text ""

valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 