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

personaFieldNames :: [String]
personaFieldNames = ["id", "description"]

goalTableName :: String
goalTableName = "goals"

goalFieldNames :: [String]
goalFieldNames = ["id", "goal"]

createPersonaRepository :: Connection -> Repository Persona PersonaData
createPersonaRepository conn = Repository {
    getAll_ = getAll' conn personaFieldNames personaTableName personaFromFields,
    get_ = get' conn personaFieldNames personaTableName personaFromFields,
    delete_ = delete' conn personaTableName,

    create_ = createPersona conn,
    update_ = updatePersona conn
}

createGoalRepository :: Connection -> Repository Goal GoalData
createGoalRepository conn = Repository {
    getAll_ = getAll' conn goalFieldNames goalTableName goalFromFields,
    get_ = get' conn goalFieldNames goalTableName goalFromFields,
    delete_ = delete' conn goalTableName,

    create_ = createGoal conn,
    update_ = updateGoal conn
}

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS personas (id INTEGER PRIMARY KEY, description TEXT)"
    execute_ conn
        "CREATE TABLE IF NOT EXISTS goals (id INTEGER PRIMARY KEY, goal TEXT)"

query' :: String -> Query
query' q = Query $ pack q

getAll' :: FromRow a => Connection -> [String] -> String -> (a -> b) -> IO [b]
getAll' conn fields table createFunc = do
    res <- query_ conn (query'
        ("SELECT " ++ intercalate ", " fields  ++ " FROM " ++ table ++ " ORDER BY id ASC"))
    return $ map createFunc res

get' :: FromRow a => Connection -> [String] -> String -> (a -> b) -> Int -> IO (Maybe b)
get' conn fields table createFunc id = do
    mt <- queryNamed conn (query'
        ("SELECT " ++ intercalate ", " fields ++ " FROM " ++ table ++ " WHERE id = :id"))
        [":id" := id]
    return $ listToMaybe $ map createFunc mt

delete' :: Connection -> String -> Int -> IO ()
delete' conn table id = do
    executeNamed conn (query'
        ("DELETE FROM " ++ table ++ " WHERE id = :id"))
        [":id" := id]

createPersona :: Connection -> PersonaData -> IO Persona
createPersona conn t@(PersonaData description) = withExclusiveTransaction conn $ do
    execute conn
        "INSERT INTO personas (description) VALUES (?)"
        (Only description)
    rowId <- lastInsertRowId conn
    return $ Persona (fromIntegral rowId) t

updatePersona :: Connection -> Int -> PersonaData -> IO ()
updatePersona conn id (PersonaData description) = do
    executeNamed conn
        "UPDATE personas SET description = :description WHERE id = :id"
        [":id" := id, ":description" := description]

createGoal :: Connection -> GoalData -> IO Goal
createGoal conn t@(GoalData goal) = withExclusiveTransaction conn $ do
    execute conn
        "INSERT INTO goals (goal) VALUES (?)"
        (Only goal)
    rowId <- lastInsertRowId conn
    return $ Goal (fromIntegral rowId) t

updateGoal :: Connection -> Int -> GoalData -> IO ()
updateGoal conn id (GoalData goal) = do
    executeNamed conn
        "UPDATE goals SET goal = :goal WHERE id = :id"
        [":id" := id, ":goal" := goal]