module Main exposing (main)

import Base exposing (..)
import Api exposing (..)
import View exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing(..)
import Maybe exposing (..)

type alias Model =
    { personas: RequestStatus (List Data)
    , goals: RequestStatus (List Data)
    , experts: RequestStatus (List Data)
    , steps: RequestStatus (List Data)
    , avoids: RequestStatus (List Data)
    , formats: RequestStatus (List Data)
    , selectedPersona: Maybe Data
    , selectedGoal: Maybe Data
    , selectedExpert: Maybe Data
    , selectedSteps: Maybe Data
    , selectedAvoid: Maybe Data
    , selectedFormat: Maybe Data
    , newPersona: String
    , newGoal: String
    , newExpert: String
    , newSteps: String
    , newAvoid: String
    , newFormat: String
    , page : Page
    }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

initModel : (Model, Cmd Msg)
initModel = 
    (
        { personas = Loading
        , goals = Loading
        , experts = Loading
        , steps = Loading
        , avoids = Loading
        , formats = Loading
        , selectedPersona = Nothing
        , selectedGoal = Nothing
        , selectedExpert = Nothing
        , selectedSteps = Nothing
        , selectedAvoid = Nothing
        , selectedFormat = Nothing
        , newPersona = ""
        , newGoal = ""
        , newExpert = ""
        , newSteps = ""
        , newAvoid = ""
        , newFormat = ""
        , page = HomePage
        }
    , getPersonas Nothing
    )

view : Model ->  Html Msg
view model = 
    case model.page of
        HomePage -> navBar (
            div []
                [
                div [] -- personas
                    [
                        viewPersonas model.personas model.selectedPersona
                    ]
                , div [] -- goals
                    [
                        viewGoals model.goals model.selectedGoal
                    ]
                , div [] -- experts
                    [
                        viewExperts model.experts model.selectedExpert
                    ]
                , div [] -- steps
                    [
                        viewSteps model.steps model.selectedSteps
                    ]
                , div [] -- avoids
                    [
                        viewAvoids model.avoids model.selectedAvoid
                    ]
                , div [] -- formats
                    [
                        viewFormats model.formats model.selectedFormat
                    ]
                , button [ class "button m-3" ] [ text "Send request to Chat GPT" ]
                ])
        EditPage -> navBar (
            div []
                [ addForm "Persona" "personas" model.newPersona NewPersonaChanged SaveNew
                , addForm "Goal" "goals" model.newGoal NewGoalChanged SaveNew
                , addForm "Expert" "experts" model.newExpert NewExpertChanged SaveNew
                , addForm "Steps" "steps" model.newSteps NewStepsChanged SaveNew
                , addForm "Avoid" "avoids" model.newAvoid NewAvoidChanged SaveNew
                , addForm "Format" "formats" model.newFormat NewFormatChanged SaveNew
                ])

update : Msg ->  Model ->  (Model, Cmd Msg)
update msg model = 
    case (model.page, msg) of
        (_, NavigateTo EditPage) -> ( { model | page = EditPage }, Cmd.none )
        (_, NavigateTo HomePage) -> ( { model | page = HomePage }, getPersonas Nothing )
        (HomePage, _) -> updateHomePage msg model
        (EditPage, _) -> updateEditPage msg model

updateHomePage : Msg -> Model -> (Model, Cmd Msg)
updateHomePage msg model =
    case msg of
        PersonasReceived p -> ( {model | personas = p }, getGoals Nothing )
        GoalsReceived g -> ( {model | goals = g }, getExperts Nothing )
        ExpertsReceived e -> ( {model | experts = e }, getSteps Nothing )
        StepsReceived s -> ( {model | steps = s }, getAvoids Nothing )
        AvoidsReceived a -> ( {model | avoids = a }, getFormats Nothing )
        FormatsReceived f -> ( {model | formats = f }, Cmd.none )
        UpdatePersona x -> ( {model | personas = updateData x model.personas}, Cmd.none )
        UpdateGoal x -> ( {model | goals = updateData x model.goals}, Cmd.none )
        UpdateExpert x -> ( {model | experts = updateData x model.experts}, Cmd.none )
        UpdateSteps x -> ( {model | steps = updateData x model.steps}, Cmd.none )
        UpdateAvoid x -> ( {model | avoids = updateData x model.avoids}, Cmd.none )
        UpdateFormat x -> ( {model | formats = updateData x model.formats}, Cmd.none )
        SavePersona x -> (model, updatePersona x)
        SaveGoal x -> (model, updateGoal x)
        SaveExpert x -> (model, updateExpert x)
        SaveSteps x -> (model, updateSteps x)
        SaveAvoid x -> (model, updateAvoid x)
        SaveFormat x -> (model, updateFormat x)
        DeletePersona x -> (model, deletePersona x)
        DeleteGoal x -> (model, deleteGoal x)
        DeleteExpert x -> (model, deleteExpert x)
        DeleteSteps x -> (model, deleteSteps x)
        DeleteAvoid x -> (model, deleteAvoid x)
        SelectPersona x -> ( {model | selectedPersona = Just x}, Cmd.none )
        SelectGoal x -> ( {model | selectedGoal = Just x}, Cmd.none )
        SelectExpert x -> ( {model | selectedExpert = Just x}, Cmd.none )
        SelectSteps x -> ( {model | selectedSteps = Just x}, Cmd.none )
        SelectAvoid x -> ( {model | selectedAvoid = Just x}, Cmd.none )
        SelectFormat x -> ( {model | selectedFormat = Just x}, Cmd.none )
        DataUpdated -> (model, getPersonas Nothing)
        _ -> (model, Cmd.none)

updateEditPage : Msg -> Model -> (Model, Cmd Msg)
updateEditPage msg model =
    case msg of
        NewPersonaChanged s -> ( {model | newPersona = s}, Cmd.none )
        NewGoalChanged s -> ( {model | newGoal = s}, Cmd.none )
        NewExpertChanged s -> ( {model | newExpert = s}, Cmd.none )
        NewStepsChanged s -> ( {model | newSteps = s}, Cmd.none )
        NewAvoidChanged s -> ( {model | newAvoid = s}, Cmd.none )
        NewFormatChanged s -> ( {model | newFormat = s}, Cmd.none )
        SaveNew url s -> ( { model
            | newPersona = ""
            , newGoal = ""
            , newExpert = ""
            , newSteps = ""
            , newAvoid = ""
            , newFormat = ""}, newData url s )
        _ -> (model, Cmd.none)

updateData : Data -> RequestStatus (List Data) -> RequestStatus (List Data)
updateData data list = case list of
    Done l -> Done (List.map (\x -> if x.id == data.id then data else x) l)
    _ -> Done []