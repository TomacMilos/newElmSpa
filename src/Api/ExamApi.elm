module Api.ExamApi exposing (Exam, decoder, Exams ,get, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Api.CourseApi exposing (Course)
import Api.ExamPeriodApi exposing (ExamPeriod)
import Api.StudentApi exposing (Student)
import Json.Decode exposing (Decoder)
import Json.Decode exposing (nullable)
import Time
import Iso8601





type alias Exams = 
    List Exam

type alias Exam = 
    {
        id : Int,
        examPoints: Int,
        labPoints: Int,
        date: Time.Posix,
        course : Course,
        examPeriod: ExamPeriod,
        student : Maybe Student
    }

examsDecoder : Decoder Exams
examsDecoder =
  Json.list decoder

decoder : Json.Decoder Exam
decoder =
    Utils.Json.record Exam
        |> withField "id" Json.int
        |> withField "examPoints" Json.int
        |> withField "labPoints" Json.int
        |> withField "date" Iso8601.decoder
        |> withField "course" Api.CourseApi.decoder
        |> withField "examPeriod" Api.ExamPeriodApi.decoder
        |> withField "student" (nullable Api.StudentApi.decoder)


get :
    { onResponse : Data Exams -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/exams/all"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
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