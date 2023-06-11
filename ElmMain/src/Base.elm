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

type alias Data =
    { id: Int
    , description : String
    }

type RequestStatus a = Loading | Failed Http.Error | Done a
