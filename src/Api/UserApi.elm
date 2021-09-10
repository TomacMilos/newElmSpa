module Api.UserApi exposing
    ( User
    , decoder, encode, Login, authentication
    )

import Api.Data exposing (Data)
import Api.Token exposing (Token)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Utils.Json

type alias User =
    { token : Token 
    , role : String
    }

type alias Login =
    { username : String 
    , password : String
    }
    
decoder : Json.Decoder User
decoder =
    Json.map2 User
        (Json.field "jwt" Api.Token.decoder)
        (Json.field "uloga" Json.string)

encode : User -> Json.Value
encode user =
    Encode.object
        [ ( "role", Encode.string user.role )
        , ( "token", Api.Token.encode user.token )
        ]


encodeLogin : Login -> Json.Value
encodeLogin login =
    Encode.object
        [ ( "username", Encode.string login.username )
        , ( "password", Encode.string  login.password )
        ]

authentication :
    { login : { login | username : String, password : String }
    , onResponse : Data User -> msg
    }
    -> Cmd msg

authentication options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "login"
                  , Encode.object
                        [ ( "email", Encode.string options.login.username )
                        , ( "password", Encode.string options.login.password )
                        ]
                  )
                ]
    in
    Http.post
        { url = "http://localhost:8080/authenticate"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse
                (decoder)
        }