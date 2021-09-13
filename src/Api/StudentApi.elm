module Api.StudentApi exposing (Student, decoder, Students ,get, delete, create, getById, update)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode




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

create :
    { student :
        { student
            | firstName : String
            , lastName : String
            , password : String
        }
    , onResponse : Data Student -> msg
    }
    -> Cmd msg

create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "firstName", Encode.string options.student.firstName )
                        , ( "lastName", Encode.string options.student.lastName )
                        , ( "password", Encode.string options.student.password)
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/user/registerStudent/"++options.student.password++"/"++options.student.firstName ++ "/"++options.student.lastName
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
getById :
    {   studentID : String,
        onResponse : Data Student -> msg
    }
    -> Cmd msg
getById options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }

update :
    { 
     studentDTO :
        { studentDTO
            | firstName : String
            , id : Int
            , lastName: String
            , cardNumber: String
        }
    , onResponse : Data Student -> msg
    }
    -> Cmd msg
update options =
    let
        body : Json.Value
        body =
            Encode.object
                [ 
                        ( "id", Encode.int options.studentDTO.id )
                        ,( "firstName", Encode.string options.studentDTO.firstName )
                        ,( "lastName", Encode.string options.studentDTO.lastName )
                        ,( "cardNumber", Encode.string options.studentDTO.cardNumber )
                ]
    in
    Token.put Nothing
        { url = "http://localhost:8080/api/students"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }