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
                ])
        EditPage -> navBar (
            div []
                [
                ])

update : Msg ->  Model ->  (Model, Cmd Msg)
update msg model = 
    case (model.page, msg) of
        (HomePage, PersonasReceived p) -> ( {model | personas = p }, getGoals Nothing )
        (HomePage, GoalsReceived g) -> ( {model | goals = g }, getExperts Nothing )
        (HomePage, ExpertsReceived e) -> ( {model | experts = e }, getSteps Nothing )
        (HomePage, StepsReceived s) -> ( {model | steps = s }, getAvoids Nothing )
        (HomePage, AvoidsReceived a) -> ( {model | avoids = a }, getFormats Nothing )
        (HomePage, FormatsReceived f) -> ( {model | formats = f }, Cmd.none )
        (HomePage, UpdatePersona x) -> ( {model | personas = updateData x model.personas}, Cmd.none )
        (HomePage, UpdateGoal x) -> ( {model | goals = updateData x model.goals}, Cmd.none )
        (HomePage, UpdateExpert x) -> ( {model | experts = updateData x model.experts}, Cmd.none )
        (HomePage, UpdateSteps x) -> ( {model | steps = updateData x model.steps}, Cmd.none )
        (HomePage, UpdateAvoid x) -> ( {model | avoids = updateData x model.avoids}, Cmd.none )
        (HomePage, UpdateFormat x) -> ( {model | formats = updateData x model.formats}, Cmd.none )
        (_, SavePersona x) -> (model, updatePersona x)
        (_, SaveGoal x) -> (model, updateGoal x)
        (_, SaveExpert x) -> (model, updateExpert x)
        (_, SaveSteps x) -> (model, updateSteps x)
        (_, SaveAvoid x) -> (model, updateAvoid x)
        (_, SaveFormat x) -> (model, updateFormat x)
        (_, NavigateTo page) -> ( { model | page = page }, Cmd.none )
        (_, _) -> (model, Cmd.none)

updateData : Data -> RequestStatus (List Data) -> RequestStatus (List Data)
updateData data list = case list of
    Done l -> Done (List.map (\x -> if x.id == data.id then data else x) l)
    _ -> Done []