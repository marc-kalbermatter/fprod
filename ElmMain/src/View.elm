module View exposing (..)

import Base exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, value, type_)
import Html.Events exposing (onClick, onInput)

navBar : Html Msg -> Html Msg
navBar body = div []
    [ nav [ class "navbar navbar-light bg-light navbar-expand-lg mb-5" ]
        [ div [ class "collapse navbar-collapse" ]
            [ a [ class "nav-item nav-link", onClick (NavigateTo HomePage) ] [ text "Home" ]
            , a [ class "nav-item nav-link", onClick (NavigateTo EditPage) ] [ text "Add New" ]
            ]
        ]
        , div [ class "m-3" ] [ body ]
    ]

addForm : String -> String -> String -> (String -> Msg) -> (String -> String -> Msg) -> Html Msg
addForm title url data update save = div []
    [ h3 [] [ text title]
    , input [ type_ "text", value data, onInput update ] []
    , button [ onClick (save url data) ] [ text "Save" ]
    ]

viewPersonas : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewPersonas personas selected = div []
    [ h3 [ class "m-3" ] [ text ("Persona: " ++ selectedString selected) ]
    , whenDone personas (viewList UpdatePersona SavePersona DeletePersona SelectPersona)
    ]

viewGoals : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewGoals goals selected = div []
    [ h3 [ class "m-3" ] [ text ("Goal: " ++ selectedString selected) ]
    , whenDone goals (viewList UpdateGoal SaveGoal DeleteGoal SelectGoal)
    ]

viewExperts : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewExperts experts selected = div []
    [ h3 [ class "m-3" ] [ text ("Expert: " ++ selectedString selected) ]
    , whenDone experts (viewList UpdateExpert SaveExpert DeleteExpert SelectExpert)
    ]

viewSteps : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewSteps steps selected = div []
    [ h3 [ class "m-3" ] [ text ("Steps: " ++ selectedString selected) ]
    , whenDone steps (viewList UpdateSteps SaveSteps DeleteSteps SelectSteps)
    ]

viewAvoids : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewAvoids avoids selected = div []
    [ h3 [ class "m-3" ] [ text ("Avoid: " ++ selectedString selected) ]
    , whenDone avoids (viewList UpdateAvoid SaveAvoid DeleteAvoid SelectAvoid)
    ]

viewFormats : RequestStatus (List Data) -> Maybe Data -> Html Msg
viewFormats formats selected = div []
    [ h3 [ class "m-3" ] [ text ("Format: " ++ selectedString selected) ]
    , whenDone formats (viewList UpdateFormat SaveFormat DeleteFormat SelectFormat)
    ]

viewList : (Data -> Msg) -> (Data -> Msg) -> (Data -> Msg) -> (Data -> Msg) -> List Data -> Html Msg
viewList update save delete select list = table [] (List.map (\x ->
    tr [] [ viewData x update save delete select ]) list)

viewData : Data -> (Data -> Msg) -> (Data -> Msg) -> (Data -> Msg) -> (Data -> Msg) -> Html Msg
viewData data update save delete select = tr []
    [ input [ value data.description, onInput (\x -> update (Data data.id x)), class "input bg-light m-2" ] [ ] 
    , button [ onClick (save data), class "button m-2"] [ text "Save" ]
    , button [ onClick (delete data), class "button m-2" ] [ text "Delete" ]
    , button [ onClick (select data), class "button m-2" ] [ text "Select" ]
    ]

whenDone : RequestStatus a -> (a -> Html Msg) -> Html Msg
whenDone s render = case s of
  Done x -> render x
  Loading -> div [] [ text "Loading ..." ]
  Failed err -> div [] [ text ("Request failed: " ++ Debug.toString err) ]

selectedString : Maybe Data -> String
selectedString s = case s of
    Nothing -> "nothing selected"
    Just d -> d.description