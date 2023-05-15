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

newtype GoalData = GoalData {
    goal :: String
} deriving(Show)

data Goal = Goal {
    goalId :: Int,
    goalData :: GoalData
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

instance FromJSON GoalData where
    parseJSON (Object o) =
        GoalData <$> o .: "goal"
    parseJSON _ = mzero

instance ToJSON GoalData where
    toJSON (GoalData goal) =
        object ["goal" .= goal]

instance FromJSON Goal where
    parseJSON (Object o) =
        Goal <$> o .: "id" <*> o .: "goal"
    parseJSON _ = mzero

instance ToJSON Goal where
    toJSON (Goal id (GoalData goal)) =
        object ["id" .= id, "goal" .= goal]

personaFromFields :: (Int, String) -> Persona
personaFromFields (id, descr) = Persona id $ PersonaData descr

goalFromFields :: (Int, String) -> Goal
goalFromFields (id, goal) = Goal id $ GoalData goal

data Repository type' data' = Repository {
    getAll_ :: IO [type'],
    get_ :: Int -> IO (Maybe type'),
    create_ :: data' -> IO type',
    update_ :: Int -> data' -> IO (),
    delete_ :: Int -> IO ()
}