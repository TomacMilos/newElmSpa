module Api.AdminApi exposing (User,create)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode
import Bootstrap.Form.Input exposing (password)
import Json.Decode exposing (Decoder)

type alias User  = 
        {
        id : Int,
        username: String,
        password:  String
        }

decoder : Decoder User
decoder =
    Json.map3 User
        (Json.field "id" Json.int)
        (Json.field "username" Json.string)
        (Json.field "password" Json.string)


create :
    { admin :
        { admin
            | username : String,
            password : String
        }
    , onResponse : Data User -> msg
    }
    -> Cmd msg
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "username", Encode.string options.admin.username),
                        ("password", Encode.string options.admin.password)
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/user/registerAdmin/"++ options.admin.username ++"/"++ options.admin.password
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }