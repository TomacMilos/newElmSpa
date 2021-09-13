module Api.EnrollmentApi exposing (Enrollment, decoder, Enrollments , get, create, getCoursesForStudent, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Json.Encode as Encode
import Api.StudentApi exposing (Student)
import Api.CourseApi exposing (Course)
import Json.Decode exposing (Decoder)
import Time
import Iso8601

type alias Enrollment = 
        {
        id : Int,
        startDate: Time.Posix,
        endDate : Time.Posix,
        student: Student,
        course: Course
         }

type alias Enrollments = List Enrollment
enrollmentsDecoder : Decoder Enrollments
enrollmentsDecoder =
  Json.list decoder

decoder : Json.Decoder Enrollment
decoder =
    Utils.Json.record Enrollment
        |> withField "id" Json.int
        |> withField "startDate" Iso8601.decoder
        |> withField "endDate" Iso8601.decoder
        |> withField "student" Api.StudentApi.decoder
        |> withField "course" Api.CourseApi.decoder


get :
    {    courseID : String,
         onResponse : Data Enrollments -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/courses/" ++ options.courseID ++ "/students" 
        , expect =
            Api.Data.expectJson options.onResponse enrollmentsDecoder
        }

delete :
    {   examId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/exams/" ++ String.fromInt options.examId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.examId)
        }
create :
    { enrollment :
        { enrollment
            | student : Student,
              course : Course,
              startDate: String,
              endDate: String
        }
    , onResponse : Data Enrollment -> msg
    }
    -> Cmd msg
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                         ( "student", Encode.object
                                                    [ ( "id", Encode.int options.enrollment.student.id )
                                                    , ( "cardNumber", Encode.string options.enrollment.student.cardNumber )
                                                    , ( "firstName", Encode.string options.enrollment.student.firstName )
                                                    , ( "lastName", Encode.string options.enrollment.student.lastName )
                                                    ]
                    
                         ),
                         ("course", Encode.object [ 
                                    ( "id", Encode.int options.enrollment.course.id )
                                    ,( "name", Encode.string options.enrollment.course.name )]
                         ),
                        ("startDate", Encode.string options.enrollment.startDate),
                        ("endDate", Encode.string options.enrollment.endDate)

                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/enrollment"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
getCoursesForStudent :
    {    studentID : String,
         onResponse : Data Enrollments -> msg
    }
    -> Cmd msg
getCoursesForStudent options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/courses" 
        , expect =
            Api.Data.expectJson options.onResponse enrollmentsDecoder
        }