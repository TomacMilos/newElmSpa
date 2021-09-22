module Pages.Teacher.Id_ exposing (Model, Msg, page)

import Gen.Params.Teacher.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import UI
import Html exposing (..)
import Gen.Route as Route exposing (Route)
import Html.Attributes exposing (type_, class, disabled, value, style)
import Api.Data exposing (Data)
import Api.StudentApi exposing (..)
import Html.Events exposing (onInput, onClick)
import Api.CourseApi
import Bootstrap.Button as Button
import Utils.Route
import Request exposing (Request)
import Browser.Navigation exposing (Key)
import Api.TeacherApi exposing (Teacher, deleteTeacherCourse)
import Api.CourseApi exposing (Courses, Course)
import Bootstrap.Dropdown as Dropdown

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update req.key
        , view = view
        , subscriptions = subscriptions
        }

-- INIT
type alias Model =
    {
        teacher: Data Teacher,
        firstname: String,
        lastname: String,
        teacherRank: String,
        id: String,
        courses: Data Courses,
        t: Maybe (Data Teacher),
        myDrop1State : Dropdown.State,
        myDrop2State : Dropdown.State,
        showAdd: Bool,
        selectedCourse: Course,
        coursesToAdd: Data Courses,
        c: Maybe (Data Course)
    }


init : Params -> ( Model, Cmd Msg )
init params =
    ( 
        {
            c = Nothing,coursesToAdd = Api.Data.Loading,
            myDrop1State = Dropdown.initialState, 
            myDrop2State = Dropdown.initialState ,
            firstname = "", 
            lastname = "", 
            teacherRank = "Select Rank", 
            id = params.id, 
            t = Nothing, 
            teacher = Api.Data.Loading, 
            courses = Api.Data.Loading, 
            showAdd = False, 
            selectedCourse = {id = 0, name = "Select Course"}
        },
        Cmd.batch
        [ Api.CourseApi.getForTeacher
            { 
              teacherID = params.id,
              onResponse = GotCourses
            },
            Api.TeacherApi.getById
            { 
              teacherID = params.id,
              onResponse = GotTeacher
            },
            Api.CourseApi.getForTeacherAdd
            { 
              teacherID = params.id,
              onResponse = GotCoursesAdd
            }
        ] )

-- UPDATE
type Msg
    = GotTeacher (Data Teacher) 
    | GotCourses (Data Courses) 
    | GotCoursesAdd (Data Courses) 
    | MyDrop1Msg Dropdown.State 
    | MyDrop2Msg Dropdown.State  
    | ChangedUloga String 
    | ChangedLastName String 
    | ChangedFirstName String 
    |  ClickedDeleteCourse Course 
    |  DeletedCourse (Data Course) 
    | SubmittedForm 
    | Saved (Data Teacher) 
    | ChangeState Bool 
    | SelectedCourse Course 
    | AddCourse 
    | SavedCourse (Data Course)

update : Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotTeacher teacher ->
            case teacher of
                Api.Data.Success t ->
                    ( { model | firstname = t.firstName, lastname = t.lastName, teacherRank = t.teacherRank }
                    , Cmd.none)
                Api.Data.Loading ->
                    ( { model | firstname = "", lastname = "", teacherRank = "Select Rank" }
                    , Cmd.none)
                _->
                    ( { model | firstname = "", lastname = "", teacherRank = "Select Rank" }
                    , Cmd.none)
        GotCourses courses ->
            ( { model | courses = courses }
            , Cmd.none
            )
        GotCoursesAdd courses ->
            ( { model | coursesToAdd = courses }
            , Cmd.none
            )
        MyDrop1Msg state ->
            ( { model | myDrop1State = state }
            , Cmd.none
            )
        MyDrop2Msg state ->
            ( { model | myDrop2State = state }
            , Cmd.none
            )
        ChangedUloga uloga ->
            ({model | teacherRank = uloga}, Cmd.none)
        ChangedLastName lastName ->
            ({model | lastname = lastName}, Cmd.none)
        ChangedFirstName firstname ->
            ({model | firstname = firstname}, Cmd.none)
        ClickedDeleteCourse course ->
          ( model
          , deleteTeacherCourse
              { 
                teacherId = valueInt (String.toInt model.id),
                courseId = course.id
              , onResponse = DeletedCourse
              }
            )
        DeletedCourse c ->
            let
                removeCourse : List Course -> List Course
                removeCourse =
                    List.filter (\course -> Api.Data.Success course /= c)
            in
            ( case c of
                Api.Data.Success cour ->
                    { model | coursesToAdd = Api.Data.map (\cours -> cour :: cours) model.coursesToAdd ,courses = Api.Data.map removeCourse model.courses}
                _ ->
                    model
            , Cmd.none
            )     
        SubmittedForm ->
            ( model
            , Api.TeacherApi.update
                { teacherDTO =
                    { 
                      id = valueInt (String.toInt model.id),
                      firstName = model.firstname,
                      lastName = model.lastname,
                      teacherRank = model.teacherRank
                    }
                , onResponse = Saved
                }
            )
        Saved teacher ->
            ( { model | t = Just teacher }
            , case teacher of
                Api.Data.Success newTeacher ->
                    Utils.Route.navigate key
                    (Route.Teacher__Teachers)
                _ ->
                    Cmd.none
            )
        ChangeState state ->
            ( { model | showAdd = state }
            ,
                    Cmd.none
            )
        SelectedCourse course ->
            ({model | selectedCourse = course}, Cmd.none)

        AddCourse ->
            ( model
            , Api.TeacherApi.addCourse
                { teacherDTO =
                    { 
                      teacherID = model.id,
                      courseID = String.fromInt model.selectedCourse.id
                    }
                , onResponse = SavedCourse
                }
            )
        SavedCourse course ->
            let
                removeCourse : List Course -> List Course
                removeCourse =
                    List.filter (\c -> Api.Data.Success c /= course)
            in
            ( case course of
                Api.Data.Success c ->
                    { model | courses = Api.Data.map (\courses -> c :: courses) model.courses, coursesToAdd = Api.Data.map removeCourse model.coursesToAdd, selectedCourse = {id = 0 , name = "Select Course"}}
                _ ->
                    model
            , Cmd.none
            )   
             
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.myDrop1State MyDrop1Msg, Dropdown.subscriptions model.myDrop2State MyDrop2Msg ]

valueInt: Maybe Int -> Int
valueInt broj =
  case broj of
    Nothing -> 0
    Just value -> value 

-- VIEW
view : Model -> View Msg
view model =
      { title = "Profesor"
    , body = UI.layout [ div [class "container"] [
            br[][],
            h2[class "ml"][text "Profesor"]
            ,div[class "mt-5"][
                div[class "form-group"][
                    label[][text "Ime"],
                    input[type_ "text", class "form-control", value model.firstname, onInput ChangedFirstName ][]
                ],
                div[class "form-group"][
                    label[][text "Prezime"],
                    input[type_ "text", class "form-control", value model.lastname, onInput ChangedLastName][]
                ],
                div[class "form-group"][
                    label[][text "Uloga"],
                    div []
                    [ Dropdown.dropdown
                        model.myDrop1State
                        { options = []
                        , toggleMsg = MyDrop1Msg
                        , toggleButton =
                        Dropdown.toggle [Button.outlineInfo ] [ text model.teacherRank]
                        , items =
                        [ Dropdown.buttonItem [ onClick (ChangedUloga "Profesor")  ] [ text "Profesor" ]
                        , Dropdown.buttonItem [ onClick (ChangedUloga "Asistent") ] [ text "Asistent" ]
                        , Dropdown.buttonItem [ onClick (ChangedUloga "Demonstrator") ] [ text "Demonstrator" ]
                        ]
                        }
                    ]
                ]
            ],
            div[class "modal-footer"][
                okButton model
            ],
            div[class "container mt-4"][
                table[class "table table-hover table-striped"][
                    thead[class "thead-dark"][
                        th[class "text-center"][text "Kursevi"],
                        th[class "text-center", style "width" "20%"][
                            button[class "btn btn-primary", onClick (ChangeState True)][ Html.i [ Html.Attributes.class "fas fa-plus"][]]
                        ]
                    ], viewCourses model,
                    addCourse model
                ]
            ]
    ] ]
    }

addCourse: Model -> Html Msg
addCourse model =
    if model.showAdd then
        tr[][
            td[class "text-center"] [
                   div []
                    [ Dropdown.dropdown
                        model.myDrop2State
                        { options = []
                        , toggleMsg = MyDrop2Msg
                        , toggleButton =
                        Dropdown.toggle [Button.outlineInfo ] [ text model.selectedCourse.name]
                        , items = courseList model
                        }
                    ]
            ],
            td[class "text-center"] [
                addButton model,
                button[class "btn btn-danger", onClick (ChangeState False)][text "X"]
            ]

         ]
    else
        text ""

courseList : Model -> List (Dropdown.DropdownItem Msg)
courseList model =
  case model.coursesToAdd of
    Api.Data.Success courses ->
      (List.map(\s -> Dropdown.buttonItem [ onClick (SelectedCourse s) ] [ text  s.name]) courses)
    Api.Data.Loading ->
      []
    _ ->
      []

viewCourses : Model -> Html Msg
viewCourses model =
  case model.courses of
    Api.Data.Success courses ->
        tbody []
        (List.map(\c -> tr [class "text-center"][
            td[class "text-center"] [text c.name],  
            td[class "text-center"][button[class "btn btn-danger", onClick (ClickedDeleteCourse c)][
               Html.i [ Html.Attributes.class "fas fa-trash"][] ]]
            ]) courses)
    Api.Data.Loading ->
      tr[][
      div[class "text-center mt-5 ml-5"][
              Html.i [ Html.Attributes.class "fas fa-circle-notch fa-5x fa-spin ml-5" ][]
      ]
      ]
    _ ->
      text "Fail"

okButton : Model -> Html Msg
okButton model =
    if  model.firstname == "" || model.lastname == "" || model.teacherRank == "Select Rank"  then
        div[][
            div[][text "Molimo unesite sve podatke!"],
            button[class "btn btn-success float-right" , disabled True][ Html.i [ Html.Attributes.class "fas fa-save"][]]
        ]
    else
        div[][
            button[class "btn btn-success", onClick SubmittedForm][ Html.i [ Html.Attributes.class "fas fa-save"][]]
        ]

addButton: Model -> Html Msg
addButton model = 
    if model.selectedCourse.id == 0 then 
        button[class "btn btn-success mr-2", disabled True][ Html.i [ Html.Attributes.class "fas fa-plus"][]]
    else
        button[class "btn btn-success mr-2", onClick AddCourse][ Html.i [ Html.Attributes.class "fas fa-plus"][]]
        
        