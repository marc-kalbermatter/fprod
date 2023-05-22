module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Char
import String


type alias Model =
    { personaText: String
    , goalText: String
    , suggestions: List String
    , currentFocus: Maybe InputField
    }

initModel : Model
initModel = 
    { personaText = ""
    , goalText = ""
    , suggestions = []
    , currentFocus = Nothing
    }

type Msg
    = FieldChange InputField String
    | FocusReceived InputField
    | FocusLost

type InputField
    = PersonaField
    | GoalField

view : Model ->  Html Msg
view model = div []
    [ div []  -- chat backlog
        [

        ]
    , div [] -- input fields
        [ div []
            [ text "Persona"
            , input [ placeholder "", value model.personaText, onInput (FieldChange PersonaField), onFocus (FocusReceived PersonaField), onBlur (FocusLost) ] []
            ]
        , div [] 
            [ text "Goal"
            , input [ placeholder "", value model.goalText, onInput (FieldChange GoalField), onFocus (FocusReceived GoalField), onBlur (FocusLost) ] []
            ]
        ] 
    , div [] -- suggestions
        [ text "Suggestions"
        , div [] 
            (List.map (\s -> div [] [ text s ]) (model.suggestions))
        ]
    ]


update : Msg ->  Model ->  Model
update msg model = case msg of
    -- UsernameFieldChange newValue -> { model | username = String.filter isAllowedChar newValue }
    FieldChange field newValue -> fieldChangeEvent field newValue model
    FocusReceived field -> focusReceivedEvent field model
    FocusLost -> focusLostEvent model


fieldChangeEvent : InputField -> String -> Model -> Model
fieldChangeEvent field newValue model = 
    case field of
        PersonaField -> { model | personaText = newValue }
        GoalField -> { model | goalText = newValue }

focusReceivedEvent : InputField -> Model -> Model
focusReceivedEvent field model = 
    { model | currentFocus = Just field, suggestions = getSuggestionsForField model field }

focusLostEvent : Model -> Model
focusLostEvent model = 
    { model | currentFocus = Nothing, suggestions = [] }

-- isAllowedChar : Char -> Bool
-- isAllowedChar char =
--     Char.isAlphaNum char || List.member char otherAllowedChars
-- otherAllowedChars = ['.', '-']


getSuggestionsForField : Model -> InputField -> List String
getSuggestionsForField model field = 
    case field of
        PersonaField -> 
            [ "Persona 1", "Persona 2", "Persona 3" ]
        GoalField ->
            [ "Goal 1", "Goal 2", "Goal 3" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }

