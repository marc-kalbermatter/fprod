module PromptEditor.Application where

import PromptEditor.Environment
import PromptEditor.Types
import Control.Monad.Reader

getAllAction :: (Env -> Repository type' data') -> App [type']
getAllAction rep = do
    rep' <- asks rep
    liftIO $ getAll_ rep'

getAction :: (Env -> Repository type' data') -> Int -> App (Maybe type')
getAction rep id = do
    rep' <- asks rep
    liftIO $ get_ rep' id

createAction :: (Env -> Repository type' data') -> data' -> App type'
createAction rep data' = do
    rep' <- asks rep
    liftIO $ create_ rep' data'

updateAction :: (Env -> Repository type' data') -> Int -> data' -> App ()
updateAction rep id data' = do
    rep' <- asks rep
    lift $ update_ rep' id data'

deleteAction :: (Env -> Repository type' data') -> Int -> App ()
deleteAction rep id = do
    rep' <- asks rep
    lift $ delete_ rep' id