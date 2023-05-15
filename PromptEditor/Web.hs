{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.Web where

import ChatGPT
import PromptEditor.Environment
import PromptEditor.Types
import PromptEditor.Application
import Database.SQLite.Simple (withConnection, Connection)
import Control.Applicative ()
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Aeson ( eitherDecode )
import Web.Scotty.Trans
import Control.Monad.Reader ( ReaderT(runReaderT), MonadTrans (lift) )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified PromptEditor.DB as DB

main :: IO ()
main = do
    config <- readConfigOrExit
    let dbFilename = filename (database config)
    withConnection dbFilename $ \conn -> do
        DB.initSchema conn
        let personaRepository = DB.createPersonaRepository conn
            goalRepository = DB.createGoalRepository conn
            chatGPTUrl = apiUrl $ chatGPT config
            chatGPTKey = apiKey $ chatGPT config
            sendRequest = makeRequest (chatGPTUrl, chatGPTKey)
            env = Env personaRepository goalRepository sendRequest
            
            runIO :: Env -> App a -> IO a
            runIO = flip runReaderT

        scottyT 4000 (runIO env) application

readConfigOrExit :: IO Config
readConfigOrExit = do
    configBytes <- BL.readFile "config.json"
    case eitherDecode configBytes of
        Left _ -> do
            hPutStrLn stderr "Failed to parse config.json"
            exitFailure
        Right config -> return config

application :: ScottyT T.Text App ()
application = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "web/build")

    get "/" $ file "web/build/index.html"

    get "/personas" $ do
        personas <- lift $ getAllAction envPersonaRepository
        json personas
    
    get "/personas/:id" $ do
        id <- param "id"
        persona <- lift $ getAction envPersonaRepository id
        json persona
    
    post "/personas" $ do
        data' <- jsonData
        persona <- lift $ createAction envPersonaRepository data'
        json (personaId persona)
    
    put "/personas/:id" $ do
        id <- param "id"
        data' <- jsonData
        lift $ updateAction envPersonaRepository id data'
    
    delete "/personas/:id" $ do
        id <- param "id"
        lift $ deleteAction envPersonaRepository id
    
    get "/goals" $ do
        goals <- lift $ getAllAction envGoalRepository
        json goals
    
    get "/goals/:id" $ do
        id <- param "id"
        goal <- lift $ getAction envGoalRepository id
        json goal
    
    post "/goals" $ do
        data' <- jsonData
        goal <- lift $ createAction envGoalRepository data'
        json (goalId goal)
    
    put "/goals/:id" $ do
        id <- param "id"
        data' <- jsonData
        lift $ updateAction envGoalRepository id data'
    
    delete "/goals/:id" $ do
        id <- param "id"
        lift $ deleteAction envGoalRepository id