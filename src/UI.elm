module UI exposing (h1, layout)

import Gen.Route as Route exposing (Route)
import Html exposing (Html)
import Html.Attributes as Attr
import Html exposing (div,header)
import Html.Attributes exposing (class)

layout : List (Html msg) -> List (Html msg)
layout children =
     let
        viewLink : String -> Route -> Html msg
        viewLink label route =
            Html.a [ Attr.href (Route.toHref route), Attr.class "nav-link" ] [ Html.text label ]
        in
    [ div [ class "nav"]
        [ header [ class "navbar navbar-toggleable-md navbar-light bg-faded" ]
            [ 
              viewLink "Studenti" Route.Student__Students   
            , viewLink "Kursevi" Route.Course__Courses  
            , viewLink "Profesori" Route.Teacher__Teachers
            , viewLink "Ispitni Rokovi" Route.ExamPeriod__ExamPeriods
            , viewLink "Dokumenti" Route.Document__Documents
            , viewLink "Uplate" Route.Payment__Payments
            , viewLink "Ispiti" Route.Exam__Exams
            , viewLink "Kreiraj Admina" Route.Admin__RegisterAdmin
            , div [class "splitter" ] [
              viewLink "Logout" Route.Home_
            ]
            ]
        , div [] []
        ]
        , Html.main_ [Attr.class "wrapper"] children

    ]

h1 : String -> Html msg
h1 label =
    Html.h1 [] [ Html.text label ]
