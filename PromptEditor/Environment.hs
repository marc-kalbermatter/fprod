{-# LANGUAGE DeriveGeneric #-}

module PromptEditor.Environment where

import ChatGPT
import PromptEditor.Types
import Data.Aeson
import GHC.Generics
import Control.Monad.Reader
import Data.Text (Text)

data Env = Env {
    envRepository :: PersonaRepository,
    envSendRequest :: [Message] -> IO ()
}

type App = ReaderT Env IO

data Config = Config {
    database :: DatabaseConfig,
    chatGPT :: ChatGPTConfig
} deriving Generic

newtype DatabaseConfig = DatabaseConfig {
    filename :: String
} deriving Generic

data ChatGPTConfig = ChatGPTConfig {
    apiUrl :: ApiUrl,
    apiKey :: ApiKey
} deriving Generic

instance FromJSON ChatGPTConfig
instance FromJSON DatabaseConfig
instance FromJSON Config