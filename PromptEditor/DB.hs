{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.DB where

import PromptEditor.Types
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Data.Aeson (Value(String))
import Data.Text (pack, unpack)
import Data.List (intercalate)
import Database.SQLite.Simple.ToField (ToField)

personaTableName :: String
personaTableName = "personas"

goalTableName :: String
goalTableName = "goals"

expertTableName :: String
expertTableName = "experts"

stepsTableName :: String
stepsTableName = "steps"

avoidTableName :: String
avoidTableName = "avoids"

formatTableName :: String
formatTableName = "formats"

createPersonaRepository :: Connection -> Repository Persona
createPersonaRepository conn = Repository {
    getAll_ = getAll' conn personaTableName (Persona . dataFromFields),
    get_ = get' conn personaTableName (Persona . dataFromFields),
    delete_ = delete' conn personaTableName,
    create_ = create' conn personaTableName Persona,
    update_ = update' conn personaTableName
}

createGoalRepository :: Connection -> Repository Goal
createGoalRepository conn = Repository {
    getAll_ = getAll' conn goalTableName (Goal . dataFromFields),
    get_ = get' conn goalTableName (Goal . dataFromFields),
    delete_ = delete' conn goalTableName,
    create_ = create' conn goalTableName Goal,
    update_ = update' conn goalTableName
}

createExpertRepository :: Connection -> Repository Expert
createExpertRepository conn = Repository {
    getAll_ = getAll' conn expertTableName (Expert . dataFromFields),
    get_ = get' conn expertTableName (Expert . dataFromFields),
    delete_ = delete' conn expertTableName,
    create_ = create' conn expertTableName Expert,
    update_ = update' conn expertTableName
}

createStepsRepository :: Connection -> Repository Steps
createStepsRepository conn = Repository {
    getAll_ = getAll' conn stepsTableName (Steps . dataFromFields),
    get_ = get' conn stepsTableName (Steps . dataFromFields),
    delete_ = delete' conn stepsTableName,
    create_ = create' conn stepsTableName Steps,
    update_ = update' conn stepsTableName
}

createAvoidRepository :: Connection -> Repository Avoid
createAvoidRepository conn = Repository {
    getAll_ = getAll' conn avoidTableName (Avoid . dataFromFields),
    get_ = get' conn avoidTableName (Avoid . dataFromFields),
    delete_ = delete' conn avoidTableName,
    create_ = create' conn avoidTableName Avoid,
    update_ = update' conn avoidTableName
}

createFormatRepository :: Connection -> Repository Format
createFormatRepository conn = Repository {
    getAll_ = getAll' conn formatTableName (Format . dataFromFields),
    get_ = get' conn formatTableName (Format . dataFromFields),
    delete_ = delete' conn formatTableName,
    create_ = create' conn formatTableName Format,
    update_ = update' conn formatTableName
}

createTable :: String -> Connection -> IO ()
createTable tableName conn = do
    execute_ conn (query'
        ("CREATE TABLE IF NOT EXISTS " ++ tableName ++ "(id INTEGER PRIMARY KEY, description TEXT)"))

initSchema :: Connection -> IO ()
initSchema conn = do
    createTable personaTableName conn
    createTable goalTableName conn
    createTable expertTableName conn
    createTable stepsTableName conn
    createTable avoidTableName conn
    createTable formatTableName conn

query' :: String -> Query
query' q = Query $ pack q

getAll' :: FromRow a => Connection -> String -> (a -> b) -> IO [b]
getAll' conn table createFunc = do
    res <- query_ conn (query'
        ("SELECT id, description FROM " ++ table ++ " ORDER BY id ASC"))
    return $ map createFunc res

get' :: FromRow a => Connection -> String -> (a -> b) -> Int -> IO (Maybe b)
get' conn table createFunc id = do
    mt <- queryNamed conn (query'
        ("SELECT id, description FROM " ++ table ++ " WHERE id = :id"))
        [":id" := id]
    return $ listToMaybe $ map createFunc mt

delete' :: Connection -> String -> Int -> IO ()
delete' conn table id = do
    executeNamed conn (query'
        ("DELETE FROM " ++ table ++ " WHERE id = :id"))
        [":id" := id]

create' :: Connection -> String -> (DataWithId -> a) -> Data -> IO a
create' conn table createFunc t@(Data description) = withExclusiveTransaction conn $ do
    execute conn (query'
        ("INSERT INTO " ++ table ++ "(description) VALUES (?)"))
        (Only description)
    rowId <- lastInsertRowId conn
    return $ createFunc $ DataWithId (fromIntegral rowId) t

update' :: Connection -> String -> Int -> Data -> IO ()
update' conn table id (Data description) = do
    executeNamed conn (query'
        ("UPDATE " ++ table ++ " SET description = :description WHERE id = :id"))
        [":id" := id, ":description" := description]