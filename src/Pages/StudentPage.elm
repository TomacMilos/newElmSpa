module Pages.StudentPage exposing (Model, Msg, page)

import Gen.Params.StudentPage exposing (Params)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Html exposing (text)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReplaceMe ->
            model



-- VIEW

view : Model -> View Msg
view model =
      { title = "Student"
    , body = UI.layout [ text "Student Page" ]
    }
