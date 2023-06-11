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
updatePersona data = Http.post
    { url = "http://localhost:4000/personas/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
    }

updateGoal : Data -> Cmd Msg
updateGoal data = Http.post
    { url = "http://localhost:4000/goals/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
    }

updateExpert : Data -> Cmd Msg
updateExpert data = Http.post
    { url = "http://localhost:4000/experts/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
    }

updateSteps : Data -> Cmd Msg
updateSteps data = Http.post
    { url = "http://localhost:4000/steps/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
    }

updateAvoid : Data -> Cmd Msg
updateAvoid data = Http.post
    { url = "http://localhost:4000/avoids/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
    }

updateFormat : Data -> Cmd Msg
updateFormat data = Http.post
    { url = "http://localhost:4000/formats/" ++ String.fromInt data.id
    , body = jsonBody (encodeData data)
    , expect = expectWhatever updateResultToMsg
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