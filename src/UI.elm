module UI exposing (h1, layout)

import Gen.Route as Route exposing (Route)
import Html exposing (Html)
import Html.Attributes as Attr
import Html exposing (div,header)
import Html.Attributes exposing (class)
import Html.Attributes exposing (style)

layout : List (Html msg) -> List (Html msg)
layout children =
     let
        viewLink : String -> String -> Route -> Html msg
        viewLink icon label route =
            Html.a [ Attr.href (Route.toHref route), Attr.class "nav-link" ] [
                Html.i [ Html.Attributes.class icon ][],
                 Html.text label ]
        in
    [ div [ class "nav"]
        [ header [ class "navbar navbar-toggleable-md navbar-light bg-faded" ]
            [ 
              viewLink "fas fa-user-graduate" " Studenti" Route.Student__Students   
            , viewLink "fas fa-chalkboard-teacher" " Kursevi" Route.Course__Courses  
            , viewLink "fas fa-users" " Profesori" Route.Teacher__Teachers
            , viewLink "fas fa-calendar-alt" " Ispitni Rokovi" Route.ExamPeriod__ExamPeriods
            , viewLink "fas fa-file-alt" " Dokumenti" Route.Document__Documents
            , viewLink "fas fa-money-bill-wave" " Uplate" Route.Payment__Payments
            , viewLink "fas fa-graduation-cap" " Ispiti" Route.Exam__Exams
            , viewLink "fas fa-user-plus" " Kreiraj Admina" Route.Admin__RegisterAdmin

            ],
            div [class "splitter", style "padding" "10px"] [
                  Html.a [ Attr.href (Route.toHref Route.Home_), Attr.class "nav-link" ] [
                 Html.text "Logout  ",
                Html.i [ Html.Attributes.class "fas fa-sign-out-alt"][]
                 
                 ]
            ]
        , div [] []
        ]
        , Html.main_ [Attr.class "wrapper"] children

    ]

h1 : String -> Html msg
h1 label =
    Html.h1 [] [ Html.text label ]
