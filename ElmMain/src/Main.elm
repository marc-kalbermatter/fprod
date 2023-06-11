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
    { personaText: String
    , goalText: String
    , promptText: String
    , personas: RequestStatus (List Data)
    , goals: RequestStatus (List Data)
    , experts: RequestStatus (List Data)
    , steps: RequestStatus (List Data)
    , avoids: RequestStatus (List Data)
    , formats: RequestStatus (List Data)
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
        { personaText = ""
        , goalText = ""
        , promptText = ""
        , personas = Loading
        , goals = Loading
        , experts = Loading
        , steps = Loading
        , avoids = Loading
        , formats = Loading
        , page = HomePage
        }
    , getPersonas Nothing
    )

view : Model ->  Html Msg
view model = 
    case model.page of
        HomePage -> 
            div []
                [
                div [] -- personas
                    [
                        viewPersonas model.personas
                    ]
                , div [] -- goals
                    [
                        viewGoals model.goals
                    ]
                , div [] -- experts
                    [
                        viewExperts model.experts
                    ]
                , div [] -- steps
                    [
                        viewSteps model.steps
                    ]
                , div [] -- avoids
                    [
                        viewAvoids model.avoids
                    ]
                , div [] -- formats
                    [
                        viewFormats model.formats
                    ]
                , button [ onClick (NavigateTo EditPage) ] [ text "Go to prompt page" ]
                ]
        EditPage ->
            div []
                [ div []
                    [ text "Prompt"
                    , input [ placeholder "", value model.promptText, onInput PromptChange ] []
                    ]
                , button [ onClick SavePrompt ] [ text "Save" ]
                ]

update : Msg ->  Model ->  (Model, Cmd Msg)
update msg model = 
    case (model.page, msg) of
        (HomePage, PersonasReceived p) -> ( {model | personas = p }, getGoals Nothing )
        (HomePage, GoalsReceived g) -> ( {model | goals = g }, getExperts Nothing )
        (HomePage, ExpertsReceived e) -> ( {model | experts = e }, getSteps Nothing )
        (HomePage, StepsReceived s) -> ( {model | steps = s }, getAvoids Nothing )
        (HomePage, AvoidsReceived a) -> ( {model | avoids = a }, getFormats Nothing )
        (HomePage, FormatsReceived f) -> ( {model | formats = f }, Cmd.none )
        (_, _) -> (model, Cmd.none)