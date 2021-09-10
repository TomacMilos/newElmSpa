module Api.CourseApi exposing (Course, decoder, Courses ,get, delete, create)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode


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


create :
    { course :
        { course
            | name : String
        }
    , onResponse : Data Course -> msg
    }
    -> Cmd msg
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "name", Encode.string options.course.name )
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/courses"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }