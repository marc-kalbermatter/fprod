{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PromptEditor.ChatGPT where

import PromptEditor.Types
import GHC.Generics
import Data.Text hiding (head)
import Data.Text.Encoding
import Data.Aeson
import Network.HTTP.Simple

data Message = Message {
    role :: String,
    content :: String
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

data Prompt = Prompt {
    model :: Text,
    temperature :: Float,
    message :: Message
} deriving (Show, Generic)

instance ToJSON Prompt
instance FromJSON Prompt

data APIResponse = APIResponse {
    id :: Text,
    object :: Text,
    created :: Int,
    model :: Text,
    choices :: [Message]
} deriving (Show, Generic)

instance ToJSON APIResponse
instance FromJSON APIResponse

newtype ApiUrl = ApiUrl {
    url :: Text
} deriving Generic

newtype ApiKey = ApiKey {
    key :: Text
} deriving Generic

instance FromJSON ApiUrl
instance ToJSON ApiUrl
instance FromJSON ApiKey
instance ToJSON ApiKey

getDescription :: DataWithId -> String
getDescription d = description $ PromptEditor.Types.content d

parseMessage :: PromptEditor.Types.Request -> Message
parseMessage (Request p g e s a f) = Message (description (PromptEditor.Types.content $ persona p)) content' where
    content' = "the goal is: "
        ++ getDescription (goal g)
        ++ "you are an expert in: "
        ++ getDescription (expert e)
        ++ "follow the steps: "
        ++ getDescription (steps s)
        ++ "avoid "
        ++ getDescription (avoid a)
        ++ "use the format: "
        ++ getDescription (format f)

makeRequest :: ApiUrl -> ApiKey -> Message -> IO ()
makeRequest openaiUrl openaiKey messages = do
    initReq <- parseRequest $ unpack $ "POST " <> url openaiUrl
    let prompt = Prompt "gpt-3.5-turbo" 1.0 messages
    let request = setRequestBodyJSON prompt $
                  setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 (key openaiKey)] $
                  setRequestHeader "Content-Type" ["application/json"] $
                  setRequestHeader "Accept" ["application/json"] initReq
    response <- httpJSONEither request
    case getResponseBody response of
        Left err -> putStrLn $ "Error: " ++ show err
        Right resp -> print (head . choices $ resp)