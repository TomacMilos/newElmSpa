module Pages.StudentsPages.StudentPage exposing (Model, Msg, page, ocena)

import Gen.Params.StudentsPages.StudentPage exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Api.StudentApi exposing (Student)
import Api.Data exposing (Data)
import Api.ExamApi exposing (Exams)
import Api.EnrollmentApi exposing (Enrollments)
import Api.DocumentsApi exposing (Documents)
import Api.PaymentApi exposing (Payments)
import Html exposing (..)
import Html.Attributes exposing (disabled, value, href, type_, class)
import UI
import Utils.Time
import Gen.Route as Route exposing (Route)
import Utils.Route
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Api.ExamPeriodApi exposing (ExamPeriods)
import Html.Attributes exposing (title)
import Api.ExamApi exposing (Exam)


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
        student: Data Student,
        s:  Student,
        enrollments: Data Enrollments,
        passExams: Data Exams,
        exams: Data Exams,
        nextExams: Data Exams,
        documents: Data Documents,
        payments: Data Payments,
        showSlusanje: Bool,
        showRezultati: Bool,
        showObrada: Bool,
        showNaredniIspiti: Bool,
        showDokumenti: Bool,
        showUplate: Bool,
        showPrijava: Bool,
        nextExamPeriods: Data ExamPeriods


    }


init : ( Model, Cmd Msg )
init =
    ( {
        showSlusanje = False,
        showRezultati = False,
        showObrada = False,
        showNaredniIspiti = False,
        showDokumenti = False,
        showUplate = False,
        showPrijava = False,
        student = Api.Data.Loading,
        nextExamPeriods = Api.Data.Loading,
        s = {
            id = 0,
            firstName = "",
            lastName = "",
            cardNumber = ""
        },
        enrollments = Api.Data.Loading,
        passExams = Api.Data.Loading,
        exams = Api.Data.Loading,
        nextExams = Api.Data.Loading,
        documents = Api.Data.Loading,
        payments = Api.Data.Loading


    }, Cmd.batch [
            Api.StudentApi.getById
            { 
              studentID = "1",
              onResponse = GotStudent
            },
            Api.EnrollmentApi.getCoursesForStudent
            { 
                studentID = "1",
                onResponse = GotEnrollment
            },
            Api.ExamApi.examPassStudent
            { 
                studentID = "1",
                onResponse = GotPassExams
            },
            Api.ExamApi.examStudent
            { 
                studentID = "1",
                onResponse = GotExam
            },
            Api.ExamApi.nextexemsStudent
            { 
                studentID = "1",
                onResponse = GotNextExams
            },
            Api.DocumentsApi.studentDocuments
            { 
                studentID = "1",
                onResponse = GotDocuments
            },
            Api.PaymentApi.studentPayments
            { 
                studentID = "1",
                onResponse = GotPayments
            },
            Api.ExamPeriodApi.getNext
            { 
                onResponse = GotExamPeriods
            }
    ] )



-- UPDATE


type Msg
     = GotStudent (Data Student) 
    | GotEnrollment (Data Enrollments) 
    | GotPassExams (Data Exams) 
    | GotExam (Data Exams) 
    | GotNextExams (Data Exams) 
    | GotExamPeriods (Data ExamPeriods) 
    | GotDocuments (Data Documents) 
    | GotPayments (Data Payments)
    | ChangeStateSlusanje Bool
    | ChangeStateRezultati Bool
    | ChangeStateObrada Bool
    | ChangeStateNaredni Bool
    | ChangeStateDokumenti Bool
    | ChangeStateUplate Bool
    | ChangeStatePrijava Bool
    | DeletedExams (Data Int) 
    | ClickedDeleteExams Exam


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStudent student ->
            case student of
                Api.Data.Success s ->
                    ( { model | s = s}
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | s = {id = 0 ,firstName = "", lastName = "", cardNumber = ""} }
                    , Cmd.none)
                _->
                    ( { model | s = {id = 0 ,firstName = "", lastName = "", cardNumber = ""} }
                    , Cmd.none)
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
        ChangeStateSlusanje stanje->
            ( { model | showSlusanje = stanje  }
            , Cmd.none
            )     
        ChangeStateRezultati stanje->
            ( { model | showRezultati = stanje  }
            , Cmd.none
            )
        ChangeStateObrada stanje->
            ( { model | showObrada = stanje  }
            , Cmd.none
            )
        ChangeStateNaredni stanje->
            ( { model | showNaredniIspiti = stanje  }
            , Cmd.none
            )              
        ChangeStateDokumenti stanje->
            ( { model | showDokumenti = stanje  }
            , Cmd.none
            )
        ChangeStateUplate stanje->
            ( { model | showUplate = stanje  }
            , Cmd.none
            )   
        ChangeStatePrijava stanje->
            ( { model | showPrijava = stanje  }
            , Cmd.none
            )  
        GotExamPeriods periods ->
            ( { model | nextExamPeriods = periods }
            , Cmd.none
            )
        ClickedDeleteExams exam ->
            ( model
            , Api.ExamApi.delete
                { 
                  examId = exam.id
                , onResponse = DeletedExams 
                }
                )
        DeletedExams id ->
                let
                    removeExam : Exams -> Exams
                    removeExam =
                        List.filter (\exam -> Api.Data.Success exam.id /= id)
                in
                ( { model | nextExams = Api.Data.map removeExam model.nextExams }
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
            h2[class "ml"][
                Html.i [ Html.Attributes.class "fas fa-user" ][],
                text " Student"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Index"],
                    input[type_ "text", class "form-control", disabled True, value model.s.cardNumber , disabled True ][]
                ],
                div[class "form-group"][
                    label[][text "Ime"],
                    input[type_ "text", class "form-control", value model.s.firstName , disabled True ][]
                ],
                div[class "form-group"][
                    label[][text "Prezime"],
                    input[type_ "text", class "form-control", value model.s.lastName, disabled True ][]
                ]
            ],
            div[class "container mt-4"][
                div[][
                    h3[][text ""]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateSlusanje (not model.showSlusanje))][
                        th[][text "Slusanje Kurseva"],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showSlusanje)][]]
                    ],
                    if model.showSlusanje then
                        
                        thead[class "thead-dark"][
                            th[class "text-center"][text "Naziv"],
                            th[class "text-center"][text "Pocetak Kursa"],
                            th[class "text-center"][text "Zavrsetak Kursa"],
                            th[class "text-center"][]
                        ]
                        
                    else
                        text ""
                    ,viewEnrollments model
                    
                ]
            ],
                        div[class "container mt-4"][
                div[][
                    h3[][text ""]
                ],
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStatePrijava (not model.showPrijava))][
                        th[][text "Prijava Ispita"],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showPrijava)][]]
                    ],
                    if model.showPrijava then
                        
                        thead[class "thead-dark"][
                            th[class "text-center"][text "Ispitni Rok"],
                            th[class "text-center"][text "Početak Ispitnog Roka	"],
                            th[class "text-center"][text "Kraj Ispitnog Roka"],
                            th[class "text-center"][text "Prijavi"]
                        ]
                        
                    else
                        text ""
                    ,viewPrijava model
                    
                ]
            ],
            div[class "container mt-4"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateRezultati (not model.showRezultati))][
                        th[][text "Rezultati Ispita"],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showRezultati)][]]
                    ],
                    if model.showRezultati then
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv Kursa"],
                        th[class "text-center"][text "Datum"],
                        th[class "text-center"][text "Vreme"],
                        th[class "text-center"][text "Ocena"],
                        th[][]
                    ]
                    else
                        text ""
                     ,viewPassExam model
                ]
            ],
            div[class "container mt-4"][
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateObrada (not model.showObrada))][
                        th[][text "Obrada Ispita"],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showObrada)][]]
                    ],
                    if model.showObrada then
                        thead[class "thead-dark"][
                            th[class "text-center"][text "Naziv Kursa"],
                            th[class "text-center"][text "Datum"],
                            th[class "text-center"][text "Vreme"],
                            th[class "text-center"][text "Informacije"]
                        ]
                    else
                        text ""
                    , viewExam model
                ]
            ],
            div[class "container mt-4"][
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateNaredni (not model.showNaredniIspiti))][
                        th[][text "Ispiti u Narednom Periodu"],
                        th[class "text-center"][text ""],
                        th[class "text-center"][text ""],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showObrada)][]]
                    ],
                        if model.showNaredniIspiti then
                            thead[class "thead-dark"][
                                th[class "text-center"][text "Naziv Kursa"],
                                th[class "text-center"][text "Datum"],
                                th[class "text-center"][text "Vreme"],
                                th[class "text-center"][text "Odjava"]
                                
                            ]
                        else
                            text ""
                        , viewNextExam model
                ]
            ],
            div[class "container mt-4"][
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateDokumenti (not model.showDokumenti))][
                        th[][text "Dokumenti Korisnika"],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showDokumenti)][]]
                    ],
                    if model.showDokumenti then
                        thead[class "thead-dark"][
                            th[class "text-center"][text "Naziv Dokumenta"],
                            th[][]
                            
                        ]
                    else
                        text ""
                    , viewDokumenti model
                ]
            ],
            div[class "container mt-4"][
                table[class "table table-striped"][
                    thead[class "thead-dark cursor-pointer" , onClick (ChangeStateUplate (not model.showUplate))][
                        th[][text "Uplate Korisnika"],
                        th[][],
                        th[][],
                        th[class "text-right"][Html.i [ Html.Attributes.class (showState model.showUplate)][]]
                    ],
                    if model.showUplate then
                        thead[class "thead-dark"][
                            th[class "text-center"][text "Svrha"],
                            th[class "text-center"][text "Vrednost"],
                            th[class "text-center"][text "Vreme"],
                            th[][]
                        ]
                    else
                        text ""
                    , viewUplata model,
                    setSum model
                ]
            ]
    ] ]
    }

viewPassExam : Model -> Html Msg
viewPassExam model =
  case model.passExams of
    Api.Data.Success exams ->
        if model.showRezultati   == True then
            tbody []
            (List.map(\e -> tr [class "text-center"][
                td[class "cursor-pointer"] [
                a[href (Route.toHref (Route.Exam__Id_ { id = String.fromInt e.id })), style "text-decoration" "none" , style "color" "black"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text e.course.name]]]
                ],
                td[class "cursor-pointer"] [
                a[href (Route.toHref (Route.Exam__Id_ { id = String.fromInt e.id })), style "text-decoration" "none" , style "color" "black"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatDate e.date)]]]
                ],
                td[class "cursor-pointer"] [
                a[href (Route.toHref (Route.Exam__Id_ { id = String.fromInt e.id })), style "text-decoration" "none" , style "color" "black"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatTime e.date)]]]
                ],
                td[class "cursor-pointer"] [
                a[href (Route.toHref (Route.Exam__Id_ { id = String.fromInt e.id })), style "text-decoration" "none" , style "color" "black"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text (ocena e.examPoints e.labPoints)]]]
                ],
                td[][]
                ]) exams)
        else
            text ""
    Api.Data.Loading ->
        tr[][
            td[][],
            div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"

viewUplata : Model -> Html Msg
viewUplata model =
  case model.payments of
    Api.Data.Success payments ->
        if model.showUplate then
            tbody []
            (List.map(\p -> tr [class "text-center"][
                td[] [text p.svrhaUplate],
                td[] [text (String.fromInt p.vrednostUplate)],
                td[] [text ((Utils.Time.formatDate p.date) ++ " " ++ (Utils.Time.formatTime p.date))],
                td[] []
                ]) payments)
        else
            text ""
    Api.Data.Loading ->
        tr[][
            td[][],
            div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"
viewDokumenti : Model -> Html Msg
viewDokumenti model =
  case model.documents of
    Api.Data.Success documents ->
        if model.showDokumenti then
            tbody []
            (List.map(\d -> tr [class "text-center"][
                td[] [text d.naziv],
                td[][]
                ]) documents)
        else
            text ""
    Api.Data.Loading ->
        tr[][
            div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"

viewExam : Model -> Html Msg
viewExam model =
  case model.exams of
    Api.Data.Success ex ->
        if model.showObrada == True then
                tbody []
                (List.map(\e -> tr [class "text-center"][
                    td[] [text e.course.name],
                    td[] [text (Utils.Time.formatDate e.date)],
                    td[] [text (Utils.Time.formatTime e.date)],
                    td[] [
                    button[class "btn btn-success mr-2"][Html.i [ Html.Attributes.class "fas fa-info" ][]]
                    ]
                    ]) ex)
        else 
            text ""
    Api.Data.Loading ->
        tr[][
            td[][],
            div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"


viewPrijava : Model -> Html Msg
viewPrijava model =
  case model.nextExamPeriods of
    Api.Data.Success examPeriods ->
        if model.showPrijava then
            tbody []
            (List.map(\ep -> tr [class "text-center"][
                td[] [
                div[style "display" "flex", style "justify-content" "center"][p[][text ep.name]]],
                td[class "cursor-pointer"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatDate ep.startDate)]]],
                td[class "cursor-pointer"] [
                div[style "display" "flex", style "justify-content" "center"][p[][text (Utils.Time.formatDate ep.endDate)]]],
                td[title "Prijavi ispit u ovom roku"][
                    a[href (Route.toHref (Route.StudentsPages__ExamRegistration__Id_ { id = String.fromInt ep.id })), style "text-decoration" "none" , style "color" "black"] [
                    button[class "btn btn-primary"][Html.i [ Html.Attributes.class "fas fa-check-square" ][]]]]
                ]) examPeriods)
        else 
            text ""
    Api.Data.Loading ->
      tr[][
      td[][],
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"
viewNextExam : Model -> Html Msg
viewNextExam model =
  case model.nextExams of
    Api.Data.Success ex ->
            if model.showNaredniIspiti == True then
                tbody []
                (List.map(\e -> tr [class "text-center"][
                    td[] [text e.course.name],
                    td[] [text (Utils.Time.formatDate e.date)],
                    td[] [text (Utils.Time.formatTime e.date)],
                    td[] [
                    button[class "btn btn-warning mr-2", onClick (ClickedDeleteExams e)][Html.i [ Html.Attributes.class "fas fa-times" ][]]
                    ]
                    ]) ex)
            else
                text ""
    Api.Data.Loading ->
            tr[][
            td[][],
            td[][],
            div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"

showState: Bool -> String
showState state = 
    if state == True then
        "fas fa-chevron-up"
    else
        "fas fa-chevron-down"

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
        if model.showSlusanje == True then
        tbody []
        (List.map(\s -> tr [class "text-center"][
            td[] [text s.course.name],
            td[] [text (Utils.Time.formatDate s.startDate)],
            td[] [text (Utils.Time.formatDate s.endDate)],
            td[] []
            ]) enrollments)
        else
            text ""
    Api.Data.Loading ->
        tr[][
            td[][],
            div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
        ]
        ]
    _ ->
      text "Fail"

valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 

setSum : Model -> Html Msg
setSum model =
 case model.payments of
    Api.Data.Success payments ->
        if model.showUplate then
            tr [][
            td[class "text-center"][text "Ukupno:"],
            td[class "text-center"][text (String.fromInt (List.sum (List.map (\p -> p.vrednostUplate) payments)))]
            ]
        else
            text ""
    Api.Data.Loading ->
      text ""
    _ ->
      text "Fail"