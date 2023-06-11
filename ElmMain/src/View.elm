module View exposing (..)

import Base exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)

navBar : Html Msg -> Html Msg
navBar body = div []
    [ nav [ class "navbar navbar-light bg-light navbar-expand-lg mb-5" ]
        [ div [ class "collapse navbar-collapse" ]
            [ a [ class "nav-item nav-link", onClick (NavigateTo HomePage) ] [ text "Home" ]
            , a [ class "nav-item nav-link", onClick (NavigateTo EditPage) ] [ text "Edit" ]
            ]
        ]
        , div [ class "m-3" ] [ body ]
    ]

viewPersonas : RequestStatus (List Data) -> Html Msg
viewPersonas personas = div []
    [ h3 [] [ text "Personas" ]
    , whenDone personas (viewList UpdatePersona)
    ]

viewGoals : RequestStatus (List Data) -> Html Msg
viewGoals goals = div []
    [ h3 [] [ text "Goals" ]
    , whenDone goals (viewList UpdateGoal)
    ]

viewExperts : RequestStatus (List Data) -> Html Msg
viewExperts experts = div []
    [ h3 [] [ text "Experts" ]
    , whenDone experts (viewList UpdateExpert)
    ]

viewSteps : RequestStatus (List Data) -> Html Msg
viewSteps steps = div []
    [ h3 [] [ text "Steps" ]
    , whenDone steps (viewList UpdateSteps)
    ]

viewAvoids : RequestStatus (List Data) -> Html Msg
viewAvoids avoids = div []
    [ h3 [] [ text "Avoids" ]
    , whenDone avoids (viewList UpdateAvoid)
    ]

viewFormats : RequestStatus (List Data) -> Html Msg
viewFormats formats = div []
    [ h3 [] [ text "Formats" ]
    , whenDone formats (viewList UpdateFormat)
    ]

viewList : (Data -> Msg) -> List Data -> Html Msg
viewList update list = table [] (List.map (\x ->
    tr [] [ viewData x update ]) list)

viewData : Data -> (Data -> Msg) -> Html Msg
viewData data update = tr []
    [ input [ value data.description, onInput (\x -> update (Data data.id x)) ] [ ] 
    , button [ onClick (SavePersona data) ] [ text "Save" ]
    ]

whenDone : RequestStatus a -> (a -> Html Msg) -> Html Msg
whenDone s render = case s of
  Done x -> render x
  Loading -> div [] [ text "Loading ..." ]
  Failed err -> div [] [ text ("Request failed: " ++ Debug.toString err) ]