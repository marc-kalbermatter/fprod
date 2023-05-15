{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ChatGPT where

import PromptEditor.Types
import GHC.Generics
import Data.Text hiding (head)
import Data.Text.Encoding
import Data.Aeson
import Network.HTTP.Simple

data Message = Message {
    role :: Text,
    content :: Text
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

data Prompt = Prompt {
    model :: Text,
    temperature :: Float,
    messages :: [Message]
} deriving (Show, Generic)

instance ToJSON Prompt
instance FromJSON Prompt

data APIResponse = APIResponse {
    id :: Text,
    object :: Text,
    created :: Int,
    model :: Text,
    choices :: [Choice]
} deriving (Show, Generic)

instance ToJSON APIResponse
instance FromJSON APIResponse

newtype Choice = Choice {
    message :: Message
} deriving (Show, Generic)

instance ToJSON Choice
instance FromJSON Choice

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

makeRequest :: (ApiUrl, ApiKey) -> [Message] -> IO ()
makeRequest (openaiUrl, openaiKey) messages = do
    initReq <- parseRequest $ unpack $ "POST " <> url openaiUrl
    let prompt = Prompt "gpt-3.5-turbo" 1.0 messages -- Add temperature and model here
    let request = setRequestBodyJSON prompt $ 
                  setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 (key openaiKey)] $
                  setRequestHeader "Content-Type" ["application/json"] $
                  setRequestHeader "Accept" ["application/json"] initReq
    response <- httpJSONEither request 
    case getResponseBody response of
        Left err -> putStrLn $ "Error: " ++ show err
        Right resp -> print (message . head . choices $ resp)