{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.DB where

import PromptEditor.Types
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Data.Aeson (Value(String))

createPersonaRepository :: Connection -> PersonaRepository
createPersonaRepository conn = PersonaRepository {
    getAllPersonas = getAllPersonas' conn,
    getPersona = getPersona' conn,
    createPersona = createPersona' conn,
    updatePersona = updatePersona' conn,
    deletePersona = deletePersona' conn
}

initSchema :: Connection -> IO ()
initSchema conn = do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS personas (id INTEGER PRIMARY KEY, description TEXT)"

getAllPersonas' :: Connection -> IO [Persona]
getAllPersonas' conn = do
    mt <- query_ conn
        "SELECT id, description FROM Personas ORDER BY id ASC"
    return $ map (uncurry personaFromFields) mt

getPersona' :: Connection -> Int -> IO (Maybe Persona)
getPersona' conn id = do
    mt <- queryNamed conn
        "SELECT id, description FROM personas WHERE id = :id"
        [":id" := id]
    return $ listToMaybe (map (uncurry personaFromFields) mt)

updatePersona' :: Connection -> Int -> PersonaData -> IO ()
updatePersona' conn id (PersonaData description) = do
    executeNamed conn
        "UPDATE personas SET description = :description, WHERE id = :id"
        [":id" := id, ":description" := description]

deletePersona' :: Connection -> Int -> IO ()
deletePersona' conn id = do
    executeNamed conn
        "DELETE FROM personas WHERE id = :id"
        [":id" := id]

createPersona' :: Connection -> PersonaData -> IO Persona
createPersona' conn t@(PersonaData description) = withExclusiveTransaction conn $ do
    execute conn
        "INSERT INTO personas (description) VALUES (?)"
        (Only description)
    rowId <- lastInsertRowId conn
    return $ Persona (fromIntegral rowId) t