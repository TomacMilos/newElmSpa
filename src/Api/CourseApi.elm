module Api.CourseApi exposing (Course, decoder, Courses ,get, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token

type alias Courses = 
    List Course

type alias Course = 
        {
        id : Int,
        name: String
         }

decoder : Json.Decoder Course
decoder =
    Utils.Json.record Course
        |> withField "id" Json.int
        |> withField "name" Json.string


get :
    { onResponse : Data Courses -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/courses"
        , expect =
            Api.Data.expectJson options.onResponse coursesDecoder
        }
delete :
    {   courseId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/courses/" ++ String.fromInt options.courseId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.courseId)
        }

coursesDecoder : Json.Decoder Courses
coursesDecoder =
   Json.list decoder