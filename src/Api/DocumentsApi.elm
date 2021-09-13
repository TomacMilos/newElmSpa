module Api.DocumentsApi exposing (Document, decoder, Documents ,get, delete, create, studentDocuments)
import Json.Decode as Json
import Utils.Json exposing (withField)
import Api.Data exposing (Data)
import Http
import Api.Token as Token
import Api.StudentApi exposing (Student)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode




type alias Document = 
        {
        id : Int,
        naziv: String,
        student: Student
        }

type alias Documents = 
    List Document

decoder : Decoder Document
decoder =
    Json.map3 Document
        (Json.field "id" Json.int)
        (Json.field "naziv" Json.string)
        (Json.field "student" Api.StudentApi.decoder)

get :
    { onResponse : Data Documents -> msg
    }
    -> Cmd msg
get options =
        Http.get
        { url = "http://localhost:8080/api/documents/all"
        , expect =
            Api.Data.expectJson options.onResponse documentsDecoder
        }
delete :
    {   documentId : Int,
        onResponse : Data Int -> msg
    }
    -> Cmd msg
delete options =
    Token.delete Nothing
        { url = "http://localhost:8080/api/documents/" ++ String.fromInt options.documentId
        , expect =
            Api.Data.expectJson options.onResponse (Json.succeed options.documentId)
        }

documentsDecoder : Decoder Documents
documentsDecoder =
   Json.list decoder

create :
    { document :
        { document
            | naziv : String
            , student: Student
        }
    , onResponse : Data Document -> msg
    }
    -> Cmd msg
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [
                        ( "naziv", Encode.string options.document.naziv )
                        , ( "student", Encode.object
                                                    [ ( "id", Encode.int options.document.student.id )
                                                    , ( "cardNumber", Encode.string options.document.student.cardNumber )
                                                    , ( "firstName", Encode.string options.document.student.firstName )
                                                    , ( "lastName", Encode.string options.document.student.lastName )
                                                    ]
                    
                         )
                        
                  
                ]
    in
    Token.post Nothing
        { url = "http://localhost:8080/api/documents"
        , body = Http.jsonBody body
        , expect =
            Api.Data.expectJson options.onResponse decoder
        }
studentDocuments:
    {   
        studentID : String,
        onResponse : Data Documents -> msg
    }
    -> Cmd msg

studentDocuments
 options =
        Http.get
        { url = "http://localhost:8080/api/students/" ++ options.studentID ++ "/documents"
        , expect =
            Api.Data.expectJson options.onResponse documentsDecoder
        }