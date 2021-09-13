module Pages.Student.Id_ exposing (Model, Msg, page)

import Gen.Params.Student.Id_ exposing (Params)
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
import Browser.Navigation exposing (Key)
import Api.EnrollmentApi exposing (Enrollments)
import Utils.Time
import Api.ExamApi
import Api.ExamApi exposing (Exam)
import Api.ExamApi exposing (Exams)
import Api.DocumentsApi
import Api.DocumentsApi exposing (Documents)
import Api.PaymentApi
import Api.PaymentApi exposing (Payments)


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
        firstName : String,
        lastName : String,
        cardNumber: String,
        student: Data Student,
        id: String,
        s: Maybe (Data Student),
        enrollments: Data Enrollments,
        passExams: Data Exams,
        exams: Data Exams,
        nextExams: Data Exams,
        documents: Data Documents,
        payments: Data Payments

    }


init : Params -> ( Model, Cmd Msg )
init params=
    ( {
        firstName = "",
        lastName = "",
        cardNumber = "",
        student = Api.Data.Loading,
        id = params.id,
        s = Nothing,
        enrollments = Api.Data.Loading,
        passExams = Api.Data.Loading,
        exams = Api.Data.Loading,
        nextExams = Api.Data.Loading,
        documents = Api.Data.Loading,
        payments = Api.Data.Loading


    }, Cmd.batch [
            Api.StudentApi.getById
            { 
              studentID = params.id,
              onResponse = GotStudent
            },
            Api.EnrollmentApi.getCoursesForStudent
            { 
                studentID = params.id,
                onResponse = GotEnrollment
            },
            Api.ExamApi.examPassStudent
            { 
                studentID = params.id,
                onResponse = GotPassExams
            },
            Api.ExamApi.examStudent
            { 
                studentID = params.id,
                onResponse = GotExam
            },
            Api.ExamApi.nextexemsStudent
            { 
                studentID = params.id,
                onResponse = GotNextExams
            },
            Api.DocumentsApi.studentDocuments
            { 
                studentID = params.id,
                onResponse = GotDocuments
            },
            Api.PaymentApi.studentPayments
            { 
                studentID = params.id,
                onResponse = GotPayments
            }
    ] )



-- UPDATE


type Msg
    = ChangedFirstName String 
    | ChangedLastName String 
    | ChangedCardNumber String
    | GotStudent (Data Student) 
    | SubmittedForm 
    | Saved (Data Student) 
    | GotEnrollment (Data Enrollments) 
    | GotPassExams (Data Exams) 
    | GotExam (Data Exams) 
    | GotNextExams (Data Exams) 
    | GotDocuments (Data Documents) 
    | GotPayments (Data Payments) 

update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        ChangedFirstName firstName ->
            ({model | firstName = firstName}, Cmd.none)
        ChangedLastName lastName ->
            ({model | lastName = lastName}, Cmd.none)
        ChangedCardNumber cardNumber ->
            ({model | cardNumber = cardNumber}, Cmd.none)
        GotStudent student ->
            case student of
                Api.Data.Success t ->
                    ( { model | firstName = t.firstName, lastName = t.lastName, cardNumber = t.cardNumber }
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | firstName = "", lastName = "", cardNumber = "" }
                    , Cmd.none)
                _->
                    ( { model | firstName = "", lastName = "", cardNumber = "" }
                    , Cmd.none)
        SubmittedForm ->
            ( model
            , Api.StudentApi.update
                { studentDTO =
                    { 
                      id = valueInt (String.toInt model.id),
                      firstName = model.firstName,
                      lastName = model.lastName,
                      cardNumber = model.cardNumber
                    }
                , onResponse = Saved
                }
            )
        Saved student ->
            ( { model | s = Just student }
            , case student of
                Api.Data.Success newStudent ->
                    Utils.Route.navigate key
                    (Route.Student__Students)
                _ ->
                    Cmd.none
            )
        GotEnrollment enrollments ->
            ( { model | enrollments = enrollments }
            , Cmd.none
            )
        GotPassExams passExams ->
            ( { model | passExams = passExams }
            , Cmd.none
            )
        GotExam exams ->
            ( { model | exams = exams }
            , Cmd.none
            )
        GotNextExams nextExams ->
            ( { model | nextExams = nextExams }
            , Cmd.none
            ) 
        GotDocuments documents ->
            ( { model | documents = documents }
            , Cmd.none
            )
        GotPayments payments ->
            ( { model | payments = payments }
            , Cmd.none
            )         

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> View Msg
view model =
      { title = "Student"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Student"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Index"],
                    input[type_ "text", class "form-control", disabled True, value model.cardNumber , onInput ChangedCardNumber ][]
                ],
                div[class "form-group"][
                    label[][text "Ime"],
                    input[type_ "text", class "form-control", value model.firstName, onInput ChangedFirstName ][]
                ],
                div[class "form-group"][
                    label[][text "Prezime"],
                    input[type_ "text", class "form-control", value model.lastName, onInput ChangedLastName ][]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Slusanje Kurseva"]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv"],
                        th[class "text-center"][text "Pocetak Kursa"],
                        th[class "text-center"][text "Zavrsetak Kursa"]
                    ], viewEnrollments model
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Rezultati ispita"]
                ],
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv Kursa"],
                        th[class "text-center"][text "Datum"],
                        th[class "text-center"][text "Vreme"],
                        th[class "text-center"][text "Ocena"]
                    ], viewPassExam model
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Obrada rezultata"]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv Kursa"],
                        th[class "text-center"][text "Datum"],
                        th[class "text-center"][text "Vreme"],
                        th[class "text-center"][text "Informacije"]
                    ], viewExam model
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Ispiti u narednom periodu"]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv Kursa"],
                        th[class "text-center"][text "Datum"],
                        th[class "text-center"][text "Vreme"],
                        th[class "text-center"][text "Odjava"],
                        th[class "text-center"][text "Informacije"]
                        
                    ], viewNextExam model
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Dokumenti korisnika"]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv Dokumenta"]
                        
                    ], viewDokumenti model
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text "Uplate korisnika"]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Svrha"],
                        th[class "text-center"][text "Vrednost"],
                        th[class "text-center"][text "Vreme"]
                    ], viewUplata model,
                      setSum model
                ]
            ]
    ] ]
    }
viewPassExam : Model -> Html Msg
viewPassExam model =
  case model.passExams of
    Api.Data.Success exams ->
        tbody []
        (List.map(\e -> tr [class "text-center"][
            td[class "cursor-pointer"] [text e.course.name],
            td[class "cursor-pointer"] [text (Utils.Time.formatDate e.date)],
            td[class "cursor-pointer"] [text (Utils.Time.formatTime e.date)],
            td[class "cursor-pointer"] [text (ocena e.examPoints e.labPoints)]

            ]) exams)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"

viewUplata : Model -> Html Msg
viewUplata model =
  case model.payments of
    Api.Data.Success payments ->
        tbody []
        (List.map(\p -> tr [class "text-center"][
            td[] [text p.svrhaUplate],
            td[] [text (String.fromInt p.vrednostUplate)],
            td[] [text ((Utils.Time.formatDate p.date) ++ " " ++ (Utils.Time.formatTime p.date))]
            ]) payments)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"
viewDokumenti : Model -> Html Msg
viewDokumenti model =
  case model.documents of
    Api.Data.Success documents ->
        tbody []
        (List.map(\d -> tr [class "text-center"][
            td[] [text d.naziv]
            ]) documents)

    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"
viewExam : Model -> Html Msg
viewExam model =
  case model.exams of
    Api.Data.Success ex ->
        tbody []
        (List.map(\e -> tr [class "text-center"][
            td[] [text e.course.name],
            td[] [text (Utils.Time.formatDate e.date)],
            td[] [text (Utils.Time.formatTime e.date)],
            td[] [
            button[class "btn btn-success mr-2"][text "i"]
            ]
            ]) ex)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"

viewNextExam : Model -> Html Msg
viewNextExam model =
  case model.nextExams of
    Api.Data.Success ex ->
        tbody []
        (List.map(\e -> tr [class "text-center"][
            td[] [text e.course.name],
            td[] [text (Utils.Time.formatDate e.date)],
            td[] [text (Utils.Time.formatTime e.date)],
            td[] [
            button[class "btn btn-warning mr-2"][text "x"]
            ],
            td[] [
            button[class "btn btn-success mr-2"][text "i"]
            ]
            ]) ex)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"
ocena: Int -> Int -> String
ocena lab ex =
    if lab + ex < 51 then
        "5"
    else if lab + ex > 50 && lab+ex < 61 then
        "6"
    else if lab + ex > 60 && lab+ex < 71 then 
        "7"
    else if lab + ex > 70 && lab+ex < 81 then 
        "8"
    else if lab + ex > 80 && lab+ex < 91 then 
        "9"
    else 
        "10"

viewEnrollments : Model -> Html Msg
viewEnrollments model =
  case model.enrollments of
    Api.Data.Success enrollments ->
        tbody []
        (List.map(\s -> tr [class "text-center"][
            td[] [text s.course.name],
            td[] [text (Utils.Time.formatDate s.startDate)],
            td[] [text (Utils.Time.formatDate s.endDate)]
            ]) enrollments)
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"

okButton : Model -> Html Msg
okButton model =
    if  model.firstName == "" || model.lastName == "" || model.cardNumber == "" then
        div[][
            div[][text "Molimo unesite sve podatke!"],
            button[class "btn btn-success mr-2" , disabled True][text "Save"]
        ]
    else
        div[][
            button[class "btn btn-success mr-2" , onClick SubmittedForm][text "Save"]
        ]
valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 

setSum : Model -> Html Msg
setSum model =
 case model.payments of
    Api.Data.Success payments ->
        tr [][
          td[class "text-center"][text "Ukupno:"],
          td[class "text-center"][text (String.fromInt (List.sum (List.map (\p -> p.vrednostUplate) payments)))]
        ]
    Api.Data.Loading ->
      text "Loading..."
    _ ->
      text "Fail"