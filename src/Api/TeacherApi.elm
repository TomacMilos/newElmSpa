module Api.TeacherApi exposing (Teacher, decoder, Teachers ,get, delete, create)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode

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

create :
    { teacher :
        { teacher
            | firstName : String
            , lastName : String
            , teacherRank : String
            , userName : String
            , password : String

        }
    , onResponse : Data Teacher -> msg
    }
    -> Cmd msg

create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "firstName", Encode.string options.teacher.firstName )
                        , ( "lastName", Encode.string options.teacher.lastName )
                        , ( "password", Encode.string options.teacher.password)
                        , ( "teacherRank", Encode.string options.teacher.teacherRank)

                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/user/registerTeacher/"++options.teacher.userName++"/"++options.teacher.password ++ "/"++options.teacher.firstName ++ "/"++options.teacher.lastName ++ "/"++options.teacher.teacherRank
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
