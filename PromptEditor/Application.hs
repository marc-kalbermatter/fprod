module PromptEditor.Application where

import PromptEditor.Environment ( App, Env )
import PromptEditor.Types ( Repository(delete_, getAll_, get_, create_, update_), Data )
import Control.Monad.Reader ( asks, MonadIO(liftIO), MonadTrans(lift) )

getAllAction :: (Env -> Repository type') -> App [type']
getAllAction rep = do
    rep' <- asks rep
    liftIO $ getAll_ rep'

getAction :: (Env -> Repository type') -> Int -> App (Maybe type')
getAction rep id = do
    rep' <- asks rep
    liftIO $ get_ rep' id

createAction :: (Env -> Repository type') -> Data -> App type'
createAction rep data' = do
    rep' <- asks rep
    liftIO $ create_ rep' data'

updateAction :: (Env -> Repository type') -> Int -> Data -> App ()
updateAction rep id data' = do
    rep' <- asks rep
    lift $ update_ rep' id data'

deleteAction :: (Env -> Repository type') -> Int -> App ()
deleteAction rep id = do
    rep' <- asks rep
    lift $ delete_ rep' id