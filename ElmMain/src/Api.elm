module Api exposing (..)

import Base exposing (..)

import Http
import Maybe exposing (withDefault)
import Http exposing (expectJson)

import Json.Decode exposing (field)

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

resultToRequestStatus : Result Http.Error a -> RequestStatus a
resultToRequestStatus r = case r of
  Ok x -> Done x
  Err e -> Failed e

decodeList : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
decodeList decodeObj = Json.Decode.list decodeObj

decodeData : Json.Decode.Decoder Data
decodeData =
    Json.Decode.map2 Data
        (field "id" Json.Decode.int)
        (field "description" Json.Decode.string)