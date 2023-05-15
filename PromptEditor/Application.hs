module PromptEditor.Application where

import PromptEditor.Environment
import PromptEditor.Types
import Control.Monad.Reader

getAllPersonasAction :: App [Persona]
getAllPersonasAction = do
    repository <- asks envRepository
    liftIO $ getAllPersonas repository

getPersonaAction :: Int -> App (Maybe Persona)
getPersonaAction id = do
    repository <- asks envRepository
    liftIO $ getPersona repository id

createPersonaAction :: PersonaData -> App Persona
createPersonaAction data' = do
    repository <- asks envRepository
    liftIO $ createPersona repository data'

updatePersonaAction :: Int -> PersonaData -> App ()
updatePersonaAction id data' = do
    repository <- asks envRepository
    liftIO $ updatePersona repository id data'

deletePersonaAction :: Int -> App ()
deletePersonaAction id = do
    repository <- asks envRepository
    liftIO $ deletePersona repository id