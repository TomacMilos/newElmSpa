module Api.StudentApi exposing (Student, decoder, Students ,get, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token




type alias Student = 
        {
        id : Int,
        cardNumber: String,
        firstName: String,
        lastName : String
         }

type alias Students = List Student

decoder : Json.Decoder Student
decoder =
    Utils.Json.record Student
        |> withField "id" Json.int
        |> withField "cardNumber" Json.string
        |> withField "firstName" Json.string
        |> withField "lastName" Json.string


get :
    { onResponse : Data Students -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/students/all"
        , expect =
            Api.Data.expectJson options.onResponse allStudentsDecoder
        }
delete :
    {   studentId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/students/" ++ String.fromInt options.studentId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.studentId)
        }

allStudentsDecoder : Json.Decoder Students
allStudentsDecoder =
        Json.list decoder