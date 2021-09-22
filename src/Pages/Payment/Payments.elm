module Pages.Payment.Payments exposing (Model, Msg, page)

import Gen.Params.Payment.Payments exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Url exposing (Protocol(..))
import Api.Data exposing (Data)
import Api.PaymentApi exposing (..)
import Utils.Time
import Html.Events exposing (onClick)

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
      payments : Data Payments,
      sum : Int
    }

init : (Model, Cmd Msg)
init =
    ( {
        payments = Api.Data.Loading, 
        sum = 0
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotPayments
            }
        ]
    )

-- UPDATE
type Msg
  = GotPayments (Data Payments) 
  |  DeletedPayment (Data Int) 
  |  ClickedDeletePayment Payment

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    GotPayments payments ->
      ( { model | payments = payments }
        , Cmd.none
      )
    ClickedDeletePayment payment ->
          ( model
          , delete
              { 
                paymentId = payment.id
              , onResponse = DeletedPayment 
              }
            )
    DeletedPayment id ->
            let
                removePayment : Payments -> Payments
                removePayment =
                    List.filter (\payment -> Api.Data.Success payment.id /= id)
            in
            ( { model | payments = Api.Data.map removePayment model.payments }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Uplate"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-money-bill-wave" ][],
              text " Uplate"]
            ,div[class "mt-5"][
                table[class "table"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Svrha Uplate"],
                        th[class "text-center"][text "Vrednost Uplate"],
                        th[class "text-center"][text "Vreme Uplate"],
                        th[class "text-center"][text "Ime i Prezime Studenta"],
                        th[class "text-center"][
                            a [ href (Route.toHref Route.Payment__NewPayment)] [
                            button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                        ]
                    ], viewPayments model,
                      setSum model
                ]
            ]
    ] ]
    }

viewPayments : Model -> Html Msg
viewPayments model =
 case model.payments of
    Api.Data.Success payments ->
        tbody []
        (List.map(\l -> tr [class "text-center"][
            td[] [text l.svrhaUplate],  
            td[] [text (String.fromInt l.vrednostUplate)],
            td[] [text (Utils.Time.formatDate l.date)],
            td[] [text (l.student.firstName ++ " " ++ l.student.lastName )],
            td[][button[class "btn btn-danger", onClick (ClickedDeletePayment l)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]
            
            ]) payments)
    Api.Data.Loading ->
      tr[][
      td[][],
      td[][],
      div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin" ][]
      ]
      ]
    _ ->
      text "Fail"

setSum : Model -> Html Msg
setSum model =
 case model.payments of
    Api.Data.Success payments ->
        tr [][
          td[class "text-center"][text "Ukupno:"],
          td[class "text-center"][text (String.fromInt (List.sum (List.map (\p -> p.vrednostUplate) payments)))]
        ]
    Api.Data.Loading ->
      text ""
    _ ->
      text "Fail"