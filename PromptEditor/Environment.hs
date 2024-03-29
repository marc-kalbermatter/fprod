{-# LANGUAGE DeriveGeneric #-}

module PromptEditor.Environment where

import PromptEditor.ChatGPT
import PromptEditor.Types
import Data.Aeson
import GHC.Generics
import Control.Monad.Reader
import Data.Text (Text)

data Env = Env {
    envPersonaRepository :: Repository Persona,
    envGoalRepository :: Repository Goal,
    envExpertRepository :: Repository Expert,
    envStepsRepository :: Repository Steps,
    envAvoidRepository :: Repository Avoid,
    envFormatRepository :: Repository Format,
    envSendRequest :: Message -> IO ()
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