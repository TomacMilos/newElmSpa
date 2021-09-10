module Api.ExamPeriodApi exposing (ExamPeriod, decoder, ExamPeriods ,get, delete)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Iso8601
import Time



type alias ExamPeriods = 
    List ExamPeriod

type alias ExamPeriod = 
        {
        id : Int,
        name: String,
        startDate: Time.Posix,
        endDate : Time.Posix
         }

decoder : Json.Decoder ExamPeriod
decoder =
    Json.map4 ExamPeriod
        (Json.field "id" Json.int)
        (Json.field "name" Json.string)
        (Json.field "startDate" Iso8601.decoder)
        (Json.field "endDate" Iso8601.decoder)

get :
    { onResponse : Data ExamPeriods -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/examPeriods/all"
        , expect =
            Api.Data.expectJson options.onResponse examPeriodDecoder
        }
delete :
    {   examPeriodId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/examPeriods/" ++ String.fromInt options.examPeriodId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.examPeriodId)
        }

examPeriodDecoder : Json.Decoder ExamPeriods
examPeriodDecoder =
        Json.list decoder