module Api.ExamPeriodApi exposing (ExamPeriod, decoder, ExamPeriods ,get, delete, create, getById)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Iso8601
import Time
import Json.Encode as Encode




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

create :
    { examPeriod :
        { examPeriod
            | name : String,
            startDate: String,
            endDate: String
        }
    , onResponse : Data ExamPeriod -> msg
    }
    -> Cmd msg
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "name", Encode.string options.examPeriod.name),
                        ( "startDate", Encode.string options.examPeriod.startDate),
                        ( "endDate", Encode.string options.examPeriod.startDate )

                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/examPeriods"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }


getById :
    {   examPeriodId : String,
        onResponse : Data ExamPeriod -> msg
    }
    -> Cmd msg
getById options =
        Http.get
        { url = "http://localhost:8080/api/examPeriods/" ++ options.examPeriodId
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }