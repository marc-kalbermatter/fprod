module Base exposing (..)

import Http

type Page = HomePage | EditPage

type Msg
    = PromptChange String
    | SavePrompt
    | NavigateTo Page
    | PersonasReceived (RequestStatus (List Data))
    | GoalsReceived (RequestStatus (List Data))
    | ExpertsReceived (RequestStatus (List Data))
    | StepsReceived (RequestStatus (List Data))
    | AvoidsReceived (RequestStatus (List Data))
    | FormatsReceived (RequestStatus (List Data))
    | UpdatePersona Data
    | SavePersona Data
    | DeletePersona Data
    | UpdateGoal Data
    | SaveGoal Data
    | DeleteGoal Data
    | UpdateExpert Data
    | SaveExpert Data
    | DeleteExpert Data
    | UpdateSteps Data
    | SaveSteps Data
    | DeleteSteps Data
    | UpdateAvoid Data
    | SaveAvoid Data
    | DeleteAvoid Data
    | UpdateFormat Data
    | SaveFormat Data
    | DeleteFormat Data
    | SelectPersona Data
    | SelectGoal Data
    | SelectExpert Data
    | SelectSteps Data
    | SelectAvoid Data
    | SelectFormat Data
    | DataUpdated
    | NoneMsg

type alias Data =
    { id: Int
    , description : String
    }

type RequestStatus a = Loading | Failed Http.Error | Done a
