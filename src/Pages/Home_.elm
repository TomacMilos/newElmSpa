module Pages.Home_ exposing  (Model, Msg, page)
import Gen.Params.Home_ exposing (Params)
import Html
import UI
import View exposing (View)

import Page
import Request
import UI
import Shared
import View exposing (View)
import Html exposing (text)
import Html.Attributes exposing (style)
import Html exposing (Attribute)
import Html exposing (div,h1)
import Html.Attributes exposing (class)
import Html exposing (input)
import Html.Attributes exposing (id)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)
import Html exposing (button)
import Html.Events exposing (onSubmit)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT

type alias Model = 
    {
        userData : UserData,
        loggedIn : Bool
    }
type alias UserData = 
    {
        username: String,
        password: String
    }

init : Model
init = 
    {
        userData ={
            username = "",
            password = ""
        },
        loggedIn = False
    }



-- VIEW
type Msg = Submit String String

-- UPDATE
update : Msg -> Model -> Model
update msg model =
  case msg of
    Submit username password ->
        {model | userData = {username = username , password = password} , loggedIn = True}

view : Model -> View Msg
view model =
    
      { title = "Login"
    , body = UI.layout [ 
            div ([] ++ divStyle)
        [ h1 [] [ text "Sign up" ]
        , Html.form formStyle
            [
            div [class "formContent"]        
                [ div [class "form-group"]
                    [ text "Index"
                     ,input ([ id "index", type_ "text"] ++ inputStyle) []
                    ]
                ,div []
                    [ text "Password"
                    ,input ([ id "password", type_ "password"] ++ inputStyle) []
                    ]
                ,div ([]++customDiv)
                    [button ([ type_ "submit"] ++ buttonStyle)
                    [ text "Sign in" ]
                    ]
                ]
            ]
        ]
     ]
    }
divStyle : List (Attribute msg)
divStyle =
    [ style "display" "flex"
    , style "align-items" "center"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "width" "100%"
    , style "min-height" "100%"
    , style "padding" "20px"
    ]

formStyle : List (Attribute msg)
formStyle =
    [ style "border-radius" "5px"
    , style "background-color" "#92badd"
    , style "padding" "20px"
    , style "width" "300px"
    ]
inputStyle : List (Attribute msg)
inputStyle =
    [ style "display" "block"
    , style "width" "260px"
    , style "padding" "12px 20px"
    , style "margin" "8px 0"
    , style "border" "none"
    , style "border-radius" "4px"
    ]
customDiv: List (Attribute msg)
customDiv = [ style "display" "flex"
    , style "justify-content" "center"
    ]

buttonStyle : List (Attribute msg)
buttonStyle =
    [ style "width" "50%"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "16px"
    ]