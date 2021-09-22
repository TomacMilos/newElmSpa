module Pages.Document.Documents exposing (Model, Msg, page)

import Gen.Params.Document.Documents exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Html.Attributes exposing (class, href)
import Html exposing (..)
import Api.DocumentsApi exposing (..)
import Api.Data exposing (Data)
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
      documents : Data Documents
    }

init : (Model, Cmd Msg)
init =
    ( {
        documents = Api.Data.Loading
      }
    , Cmd.batch
        [ get
            { 
              onResponse = GotDocuments
            }
        ]
    )

-- UPDATE
type Msg
  = GotDocuments (Data Documents) |  DeletedDocuments (Data Int) |  ClickedDeleteDocument Document

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotDocuments documents ->
      ( { model | documents = documents }
        , Cmd.none
      )
    ClickedDeleteDocument document ->
          ( model
          , delete
              { 
                documentId = document.id
              , onResponse = DeletedDocuments 
              }
            )
    DeletedDocuments id ->
            let
                removeDocument : Documents -> Documents
                removeDocument =
                    List.filter (\document -> Api.Data.Success document.id /= id)
            in
            ( { model | documents = Api.Data.map removeDocument model.documents }
            , Cmd.none
            )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> View Msg
view model =
      { title = "Dokumenti"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][
              Html.i [ Html.Attributes.class "fas fa-file-alt" ][],
              text " Dokumenta"]
            ,div[class "mt-5"][
                table[class "table table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Naziv"],
                        th[class "text-center"][text "Ime i Prezime Studenta"],
                        th[class "text-center"][
                          a [ href (Route.toHref Route.Document__NewDocument)] [
                          button[class "btn btn-success"][Html.i [ Html.Attributes.class "fas fa-plus" ][]]]
                        ]
                    ], viewDocuments model
                ]
            ]
    ] ]
    }
viewDocuments : Model -> Html Msg
viewDocuments model =
  case model.documents of
    Api.Data.Success documents ->
        tbody []
        (List.map(\d -> tr [class "text-center"][
            td[] [text d.naziv],  
            td[] [text (d.student.firstName ++ " " ++ d.student.lastName )],
            td[][button[class "btn btn-danger", onClick (ClickedDeleteDocument d)][Html.i [ Html.Attributes.class "fas fa-trash" ][]]]

            ]) documents)
    Api.Data.Loading ->
      tr[][
      td[][],
      div[class "text-center mt-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin" ][]
      ]
      ]
    _ ->
      text "Fail"