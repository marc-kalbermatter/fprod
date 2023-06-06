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

expertInTableName :: String
expertInTableName = "expertIn"

stepsTableName :: String
stepsTableName = "steps"

avoidTableName :: String
avoidTableName = "avoid"

formatTableName :: String
formatTableName = "format"

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

createExpertInRepository :: Connection -> Repository ExpertIn
createExpertInRepository conn = Repository {
    getAll_ = getAll' conn expertInTableName (ExpertIn . dataFromFields),
    get_ = get' conn expertInTableName (ExpertIn . dataFromFields),
    delete_ = delete' conn expertInTableName,
    create_ = create' conn expertInTableName ExpertIn,
    update_ = update' conn expertInTableName
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

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS personas (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS goals (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS expertIn (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS steps (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS avoid (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS format (id INTEGER PRIMARY KEY, description TEXT)"

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