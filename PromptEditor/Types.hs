{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.Types where

import Data.Aeson
import GHC.Generics
import Control.Monad.Reader

newtype PersonaData = PersonaData {
    description :: String
} deriving (Show)

data Persona = Persona {
    personaId :: Int,
    personaData :: PersonaData
}

newtype ChatQuery = ChatQuery {
    query :: String
}

instance FromJSON PersonaData where
    parseJSON (Object o) =
        PersonaData <$> o .: "description"
    parseJSON _ = mzero

instance ToJSON PersonaData where
    toJSON (PersonaData description) =
        object ["description" .= description]

instance FromJSON Persona where
    parseJSON (Object o) =
        Persona <$> o .: "id" <*> o .: "description"
    parseJSON _ = mzero

instance ToJSON Persona where
    toJSON (Persona id (PersonaData description)) =
        object ["id" .= id, "description" .= description]

personaFromFields :: Int -> String -> Persona
personaFromFields id descr = Persona id $ PersonaData descr

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a, b) = f a b

data Env = Env {
    envRepository :: PersonaRepository,
    envSendRequest :: ChatQuery -> IO ()
}

type App = ReaderT Env IO

data PersonaRepository = PersonaRepository {
    getAllPersonas :: IO [Persona],
    getPersona :: Int -> IO (Maybe Persona),
    createPersona :: PersonaData -> IO Persona,
    updatePersona :: Int -> PersonaData -> IO (),
    deletePersona :: Int -> IO ()
}

data Config = Config {
    database :: DatabaseConfig,
    chatGPT :: ChatGPTConfig
} deriving Generic

newtype DatabaseConfig = DatabaseConfig {
    filename :: String
} deriving Generic

data ChatGPTConfig = ChatGPTConfig {
    apiUrl :: String,
    apiKey :: String
} deriving Generic

instance FromJSON ChatGPTConfig
instance FromJSON DatabaseConfig
instance FromJSON Config