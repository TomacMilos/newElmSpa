module Api.TeacherApi exposing (Teacher, decoder, Teachers ,get, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token

type alias Teachers = 
    List Teacher

type alias Teacher = 
        {
        id : Int,
        firstName: String,
        lastName: String,
        teacherRank : String
        }

decoder : Json.Decoder Teacher
decoder =
    Utils.Json.record Teacher
        |> withField "id" Json.int
        |> withField "firstName" Json.string
        |> withField "lastName" Json.string
        |> withField "teacherRank" Json.string




get :
    { onResponse : Data Teachers -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/teachers"
        , expect =
            Api.Data.expectJson options.onResponse teacherDecoder
        }
delete :
    {   teacherId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/teachers/" ++ String.fromInt options.teacherId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.teacherId)
        }

teacherDecoder : Json.Decoder Teachers
teacherDecoder =
   Json.list decoder