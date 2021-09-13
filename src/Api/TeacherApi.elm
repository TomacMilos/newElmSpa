module Api.TeacherApi exposing (Teacher, decoder, Teachers ,get, delete, create, getById, deleteTeacherCourse, update, addCourse)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode
import Api.CourseApi exposing (Course)
import Api.CourseApi exposing (Courses)

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

deleteTeacherCourse :
    {   teacherId : Int,
        courseId : Int,
        onResponse : Data Course -> msg
    }
    -> Cmd msg
deleteTeacherCourse options =
    let
        body : Json.Value
        body =
            Encode.object
                [ 
                ]
    in
    Token.put Nothing
        { url = "http://localhost:8080/api/teachers/" ++ String.fromInt options.teacherId ++ "/remove/" ++ String.fromInt options.courseId
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse Api.CourseApi.decoder
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

getById :
    {   teacherID : String,
        onResponse : Data Teacher -> msg
    }
    -> Cmd msg
getById options =
        Http.get
        { url = "http://localhost:8080/api/teachers/" ++ options.teacherID
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
update :
    { 
     teacherDTO :
        { teacherDTO
            | firstName : String
            , id : Int
            , lastName: String
            , teacherRank: String
        }
    , onResponse : Data Teacher -> msg
    }
    -> Cmd msg
update options =
    let
        body : Json.Value
        body =
            Encode.object
                [ 
                        ( "id", Encode.int options.teacherDTO.id )
                        ,( "firstName", Encode.string options.teacherDTO.firstName )
                        ,( "lastName", Encode.string options.teacherDTO.lastName )
                        ,( "teacherRank", Encode.string options.teacherDTO.teacherRank )
                ]
    in
    Token.put Nothing
        { url = "http://localhost:8080/api/teachers"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }

addCourse :
    { 
     teacherDTO :
        { teacherDTO
            | teacherID : String
            , courseID : String
        }
    , onResponse : Data Course -> msg
    }
    -> Cmd msg
addCourse options =
    let
        body : Json.Value
        body =
            Encode.object
                []
    in
    Token.put Nothing
        { url = "http://localhost:8080/api/teachers/" ++ options.teacherDTO.teacherID ++ "/course/"++ options.teacherDTO.courseID
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse Api.CourseApi.decoder
        }