{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import Data.Text (Text, unpack)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

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

data Choice = Choice {
  message :: Message
} deriving (Show, Generic)

instance ToJSON Choice
instance FromJSON Choice

openaiUrl :: Text
openaiUrl = "https://api.openai.com/v1/chat/completions"

makeRequest :: Text -> [Message] -> IO ()
makeRequest openaiKey messages = do
  initReq <- parseRequest $ unpack $ "POST " <> openaiUrl
  let prompt = Prompt "gpt-3.5-turbo" 1.0 messages -- Add temperature and model here
  let request = setRequestBodyJSON prompt $ 
                setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 openaiKey] $
                setRequestHeader "Content-Type" ["application/json"] $
                setRequestHeader "Accept" ["application/json"] initReq
  response <- httpJSONEither request 
  case getResponseBody response of
    Left err -> putStrLn $ "Error: " ++ show err
    Right resp -> print (message . head . choices $ resp)
