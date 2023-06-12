module Api exposing (..)

import Base exposing (..)

import Http exposing (..)
import Maybe exposing (withDefault)

import Json.Decode exposing (field)
import Json.Encode

getPersonas : Maybe String -> Cmd Msg
getPersonas url = Http.get
    { url = withDefault "http://localhost:4000/personas/" url
    , expect = expectJson (resultToRequestStatus >> PersonasReceived) (decodeList decodeData)
    }

getGoals : Maybe String -> Cmd Msg
getGoals url = Http.get
    { url = withDefault "http://localhost:4000/goals/" url
    , expect = expectJson (resultToRequestStatus >> GoalsReceived) (decodeList decodeData)
    }

getExperts : Maybe String -> Cmd Msg
getExperts url = Http.get
    { url = withDefault "http://localhost:4000/experts/" url
    , expect = expectJson (resultToRequestStatus >> ExpertsReceived) (decodeList decodeData)
    }

getSteps : Maybe String -> Cmd Msg
getSteps url = Http.get
    { url = withDefault "http://localhost:4000/steps/" url
    , expect = expectJson (resultToRequestStatus >> StepsReceived) (decodeList decodeData)
    }

getAvoids : Maybe String -> Cmd Msg
getAvoids url = Http.get
    { url = withDefault "http://localhost:4000/avoids/" url
    , expect = expectJson (resultToRequestStatus >> AvoidsReceived) (decodeList decodeData)
    }

getFormats : Maybe String -> Cmd Msg
getFormats url = Http.get
    { url = withDefault "http://localhost:4000/formats/" url
    , expect = expectJson (resultToRequestStatus >> FormatsReceived) (decodeList decodeData)
    }

updatePersona : Data -> Cmd Msg
updatePersona data = Http.request
    { url = "http://localhost:4000/personas/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateGoal : Data -> Cmd Msg
updateGoal data = Http.request
    { url = "http://localhost:4000/goals/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateExpert : Data -> Cmd Msg
updateExpert data = Http.request
    { url = "http://localhost:4000/experts/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateSteps : Data -> Cmd Msg
updateSteps data = Http.request
    { url = "http://localhost:4000/steps/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateAvoid : Data -> Cmd Msg
updateAvoid data = Http.request
    { url = "http://localhost:4000/avoids/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

updateFormat : Data -> Cmd Msg
updateFormat data = Http.request
    { url = "http://localhost:4000/formats/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectString updateResultToMsg
    , method = "PUT"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    }

deletePersona : Data -> Cmd Msg
deletePersona data = Http.request
    { url = "http://localhost:4000/personas/" ++ String.fromInt data.id
    , body = emptyBody
    , method = "DELETE"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectString updateResultToMsg
    }

deleteGoal : Data -> Cmd Msg
deleteGoal data = Http.request
    { url = "http://localhost:4000/goals/" ++ String.fromInt data.id
    , body = emptyBody
    , method = "DELETE"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectString updateResultToMsg
    }

deleteExpert : Data -> Cmd Msg
deleteExpert data = Http.request
    { url = "http://localhost:4000/experts/" ++ String.fromInt data.id
    , body = emptyBody
    , method = "DELETE"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectString updateResultToMsg
    }

deleteSteps : Data -> Cmd Msg
deleteSteps data = Http.request
    { url = "http://localhost:4000/steps/" ++ String.fromInt data.id
    , body = emptyBody
    , method = "DELETE"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectString updateResultToMsg
    }

deleteAvoid : Data -> Cmd Msg
deleteAvoid data = Http.request
    { url = "http://localhost:4000/avoids/" ++ String.fromInt data.id
    , body = emptyBody
    , method = "DELETE"
    , headers = []
    , timeout = Nothing
    , tracker = Nothing
    , expect = expectString updateResultToMsg
    }

resultToRequestStatus : Result Http.Error a -> RequestStatus a
resultToRequestStatus r = case r of
  Ok x -> Done x
  Err e -> Failed e

updateResultToMsg : Result Http.Error a -> Msg
updateResultToMsg r = case r of
    Ok _ -> DataUpdated
    _ -> NoneMsg

decodeList : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeList decodeObj = Json.Decode.list decodeObj

decodeData : Json.Decode.Decoder Data
decodeData =
    Json.Decode.map2 Data
        (field "id" Json.Decode.int)
        (field "description" Json.Decode.string)

encodeData : Data -> Json.Encode.Value
encodeData data =
    Json.Encode.object
        [ ("description", Json.Encode.string data.description)]