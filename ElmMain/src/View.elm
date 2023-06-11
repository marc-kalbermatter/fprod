module View exposing (..)

import Base exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

viewPersonas : RequestStatus (List Data) -> Html Msg
viewPersonas personas = div []
    [ h3 [] [ text "Personas" ]
    , whenDone personas (\x -> viewList "Persona" x)
    ]

viewGoals : RequestStatus (List Data) -> Html Msg
viewGoals goals = div []
    [ h3 [] [ text "Goals" ]
    , whenDone goals (\x -> viewList "Goal" x)
    ]

viewExperts : RequestStatus (List Data) -> Html Msg
viewExperts experts = div []
    [ h3 [] [ text "Experts" ]
    , whenDone experts (\x -> viewList "Expert" x)
    ]

viewSteps : RequestStatus (List Data) -> Html Msg
viewSteps steps = div []
    [ h3 [] [ text "Steps" ]
    , whenDone steps (\x -> viewList "Steps" x)
    ]

viewAvoids : RequestStatus (List Data) -> Html Msg
viewAvoids avoids = div []
    [ h3 [] [ text "Avoids" ]
    , whenDone avoids (\x -> viewList "Avoid" x)
    ]

viewFormats : RequestStatus (List Data) -> Html Msg
viewFormats formats = div []
    [ h3 [] [ text "Formats" ]
    , whenDone formats (\x -> viewList "Format" x)
    ]

viewList : String -> List Data -> Html Msg
viewList title list = ul [] (List.map (\x ->
    li [] [
        viewData title x
    ]) list)

viewData : String -> Data -> Html Msg
viewData title p = div []
  [ h5 [] [ text (title ++ String.fromInt p.id) ]
  , table [ class "table" ]
    [ tr []
      [ td [] [ text "Description" ], td [] [ text p.description ]
      ]
    ]
  ]

whenDone : RequestStatus a -> (a -> Html Msg) -> Html Msg
whenDone s render = case s of
  Done x -> render x
  Loading -> div [] [ text "Loading ..." ]
  Failed err -> div [] [ text ("Request failed: " ++ Debug.toString err) ]