module Main exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Char
import String

type Page = HomePage | PromptPage

type alias Model =
    { personaText: String
    , goalText: String
    , suggestions: List String
    , currentFocus: Maybe InputField
    , promptText: String
    , page : Page
    }

initModel : Model
initModel = 
    { personaText = ""
    , goalText = ""
    , suggestions = []
    , currentFocus = Nothing
    , promptText = ""
    , page = HomePage
    }

type Msg
    = FieldChange InputField String
    | PromptChange String
    | SavePrompt
    | FocusReceived InputField
    | FocusLost
    | NavigateTo Page

type InputField
    = PersonaField
    | GoalField

view : Model ->  Html Msg
view model = 
    case model.page of
        HomePage -> 
            div []
                [ div []  -- chat backlog
                    []
                , div [] -- input fields
                    [ div []
                        [ text "Persona"
                        , input [ placeholder "", value model.personaText, onInput (FieldChange PersonaField), onFocus (FocusReceived PersonaField), onBlur FocusLost ] []
                        ]
                    , div [] 
                        [ text "Goal"
                        , input [ placeholder "", value model.goalText, onInput (FieldChange GoalField), onFocus (FocusReceived GoalField), onBlur FocusLost ] []
                        ]
                    ] 
                , div [] -- suggestions
                    [ text "Suggestions"
                    , div [] 
                        (List.map (\s -> div [] [ text s ]) (model.suggestions))
                    ]
                , button [ onClick (NavigateTo PromptPage) ] [ text "Go to prompt page" ]
                ]
        PromptPage ->
            div []
                [ div []
                    [ text "Prompt"
                    , input [ placeholder "", value model.promptText, onInput PromptChange ] []
                    ]
                , button [ onClick SavePrompt ] [ text "Save" ]
                ]

update : Msg ->  Model ->  Model
update msg model = 
    case msg of
        FieldChange field newValue -> fieldChangeEvent field newValue model
        PromptChange newPrompt -> { model | promptText = newPrompt }
        SavePrompt -> savePromptEvent model
        FocusReceived field -> focusReceivedEvent field model
        FocusLost -> focusLostEvent model
        NavigateTo page -> { model | page = page }

fieldChangeEvent : InputField -> String -> Model -> Model
fieldChangeEvent field newValue model = 
    case field of
        PersonaField -> { model | personaText = newValue }
        GoalField -> { model | goalText = newValue }

savePromptEvent : Model -> Model
savePromptEvent model =
    -- In here you would normally send the persona, goal and prompt to the backend.
    -- For now, we're just logging them to the console.
    model

focusReceivedEvent : InputField -> Model -> Model
focusReceivedEvent field model = 
    { model | currentFocus = Just field, suggestions = getSuggestionsForField model field }

focusLostEvent : Model -> Model
focusLostEvent model = 
    { model | currentFocus = Nothing, suggestions = [] }

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
