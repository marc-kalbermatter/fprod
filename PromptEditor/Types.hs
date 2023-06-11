{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PromptEditor.Types where

import Data.Aeson
    ( (.:),
      object,
      FromJSON(parseJSON),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )

import Control.Monad.Reader ( MonadPlus(mzero) )

newtype Data = Data {
    description :: String
} deriving (Show)

data DataWithId = DataWithId {
    dataId :: Int,
    content :: Data
}

newtype Persona = Persona {
    persona :: DataWithId
} deriving(FromJSON, ToJSON)

newtype Goal = Goal {
    goal :: DataWithId
} deriving(FromJSON, ToJSON)

newtype Expert = Expert {
    expert :: DataWithId
} deriving(FromJSON, ToJSON)

newtype Steps = Steps {
    steps :: DataWithId
} deriving(FromJSON, ToJSON)

newtype Avoid = Avoid {
    avoid :: DataWithId
} deriving(FromJSON, ToJSON)

newtype Format = Format {
    format :: DataWithId
} deriving(FromJSON, ToJSON)

data Request = Request {
    persona_ :: Persona,
    goal_ :: Goal,
    expert_ :: Expert,
    steps_ :: Steps,
    avoid_ :: Avoid,
    format_ :: Format
}

instance FromJSON Data where
    parseJSON (Object o) =
        Data <$> o .: "description"
    parseJSON _ = mzero

instance ToJSON Data where
    toJSON (Data description) =
        object ["description" .= description]

instance FromJSON DataWithId where
    parseJSON (Object o) =
        DataWithId <$> o .: "id" <*> o .: "description"
    parseJSON _ = mzero

instance ToJSON DataWithId where
    toJSON :: DataWithId -> Value
    toJSON (DataWithId id (Data description)) =
        object ["id" .= id, "description" .= description]

dataFromFields :: (Int, String) -> DataWithId
dataFromFields (id, data_) = DataWithId id $ Data data_

data Repository type' = Repository {
    getAll_ :: IO [type'],
    get_ :: Int -> IO (Maybe type'),
    create_ :: Data -> IO type',
    update_ :: Int -> Data -> IO (),
    delete_ :: Int -> IO ()
}