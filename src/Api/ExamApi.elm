module Api.ExamApi exposing (Exam, decoder, Exams ,delete, getExamPeriodExams,get, examPassStudent, examStudent, examsForTeacher, update , nextexemsStudent, getById, registerExam, getExamsForExamPeriod)
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
import Json.Encode as Encode


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

getExamPeriodExams :
    {   
        examPeriodId : String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg
getExamPeriodExams options =
        Http.get
        { url = "http://localhost:8080/api/examPeriods/" ++ options.examPeriodId ++ "/exams"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }
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

getExamsForExamPeriod :
    { 
        examPeriodId: String,
        studentId: String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg
getExamsForExamPeriod options =
        Http.get
        { url = "http://localhost:8080/api/examPeriods/"++ options.examPeriodId ++"/exam/" ++ options.studentId
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }
getById :
    { 
        examID : String,
        onResponse : Data Exam -> msg
    }
    -> Cmd msg
getById options =
        Http.get
        { url = "http://localhost:8080/api/exams/" ++ options.examID
        , expect =
            Api.Data.expectJson options.onResponse decoder
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


examPassStudent:
    {   
        studentID : String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg
examPassStudent
 options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/examspass"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }
examStudent:
    {   
        studentID : String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg
examStudent
 options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/exams"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }

nextexemsStudent:
    {   
        studentID : String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg

nextexemsStudent
 options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/nextexems"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }
examsForTeacher:
    {   
        courseID : String,
        onResponse : Data Exams -> msg
    }
    -> Cmd msg

examsForTeacher
 options =
        Http.get
        { url = "http://localhost:8080/api/courses/" ++ options.courseID ++ "/examspasscourse"
        , expect =
            Api.Data.expectJson options.onResponse examsDecoder
        }

registerExam :
    { 
     examDTO :
        { examDTO
            | course : Course
            , date : Time.Posix
            , examPeriod: ExamPeriod
            , examPoints: Int
            , id: Int
            , labPoints: Int
            , student: Student
        }
    , onResponse : Data Exam -> msg
    }
    -> Cmd msg
    
registerExam options =
    let
        body : Json.Value
        body =
            Encode.object
                [ 
                        ( "id", Encode.int options.examDTO.id )
                        ,( "date", Iso8601.encode options.examDTO.date)
                        ,( "examPeriod", Encode.object 
                                                    [ ( "name", Encode.string options.examDTO.examPeriod.name),
                                                      ( "startDate", Iso8601.encode options.examDTO.examPeriod.startDate),
                                                      ( "endDate", Iso8601.encode options.examDTO.examPeriod.startDate)
                                                    ])
                        
                        ,( "labPoints", Encode.int options.examDTO.labPoints )
                        ,( "examPoints", Encode.int options.examDTO.examPoints)
                        ,( "student", Encode.object 
                                                    [ 
                                                    ( "id", Encode.int options.examDTO.student.id )
                                                    ,( "firstName", Encode.string options.examDTO.student.firstName )
                                                    ,( "lastName", Encode.string options.examDTO.student.lastName )
                                                    ,( "cardNumber", Encode.string options.examDTO.student.cardNumber )
                                                    ])
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/exams/" ++ String.fromInt options.examDTO.student.id ++ "/examRegistration/" ++ String.fromInt options.examDTO.id 
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        } 
update :
    { 
     examDTO :
        { examDTO
            | course : Course
            , date : Time.Posix
            , examPeriod: ExamPeriod
            , examPoints: Int
            , id: Int
            , labPoints: Int
            , student: Student
        }
    , onResponse : Data Exam -> msg
    }
    -> Cmd msg
    
update options =
    let
        body : Json.Value
        body =
            Encode.object
                [ 
                        ( "id", Encode.int options.examDTO.id )
                        ,( "date", Iso8601.encode options.examDTO.date)
                        ,( "examPeriod", Encode.object 
                                                    [ ( "name", Encode.string options.examDTO.examPeriod.name),
                                                      ( "startDate", Iso8601.encode options.examDTO.examPeriod.startDate),
                                                      ( "endDate", Iso8601.encode options.examDTO.examPeriod.startDate)
                                                    ])
                        
                        ,( "labPoints", Encode.int options.examDTO.labPoints )
                        ,( "examPoints", Encode.int options.examDTO.examPoints)
                        ,( "student", Encode.object 
                                                    [ 
                                                    ( "id", Encode.int options.examDTO.student.id )
                                                    ,( "firstName", Encode.string options.examDTO.student.firstName )
                                                    ,( "lastName", Encode.string options.examDTO.student.lastName )
                                                    ,( "cardNumber", Encode.string options.examDTO.student.cardNumber )
                                                    ])
                ]
    in
    Token.put Nothing
        { url = "http://localhost:8080/api/exams" 
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        } 