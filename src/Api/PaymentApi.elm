module Api.PaymentApi exposing (Payment, decoder, Payments ,get, delete, create, studentPayments)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Api.StudentApi exposing (Student)
import Json.Decode exposing (Decoder)
import Time
import Iso8601
import Json.Encode as Encode





type alias Payments = 
    List Payment

type alias Payment = 
        {
        id : Int,
        svrhaUplate: String,
        vrednostUplate: Int,
        date : Time.Posix,
        student: Student
         }

decoder : Decoder Payment
decoder =
    Json.map5 Payment
        (Json.field "id" Json.int)
        (Json.field "svrhaUplate" Json.string)
        (Json.field "vrednostUplate" Json.int)
        (Json.field "date" Iso8601.decoder)
        (Json.field "student" Api.StudentApi.decoder)

paymentsDecoder : Decoder Payments
paymentsDecoder =
   Json.list decoder

get :
    { onResponse : Data Payments -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/payments/all"
        , expect =
            Api.Data.expectJson options.onResponse paymentsDecoder
        }
delete :
    {   paymentId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/payments/" ++ String.fromInt options.paymentId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.paymentId)
        }

create :
    { payment :
        { payment
            | svrhaUplate : String
            , vrednostUplate : Int
            , date : String
            , student: Student
        }
    , onResponse : Data Payment -> msg
    }
    -> Cmd msg

create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "svrhaUplate", Encode.string options.payment.svrhaUplate )
                        , ( "vrednostUplate", Encode.int options.payment.vrednostUplate )
                        , ( "date", Encode.string options.payment.date)
                        , ( "student", Encode.object
                                                    [ ( "id", Encode.int options.payment.student.id )
                                                    , ( "cardNumber", Encode.string options.payment.student.cardNumber )
                                                    , ( "firstName", Encode.string options.payment.student.firstName )
                                                    , ( "lastName", Encode.string options.payment.student.lastName )
                                                    ]
                    
                         )
                        
                  
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/payments"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
studentPayments:
    {   
        studentID : String,
        onResponse : Data Payments -> msg
    }
    -> Cmd msg

studentPayments
 options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/payments"
        , expect =
            Api.Data.expectJson options.onResponse paymentsDecoder
        }