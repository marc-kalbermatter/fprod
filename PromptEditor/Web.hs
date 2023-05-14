{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.Web where

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
            env = Env personaRepository
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
        personas <- lift getAllPersonasAction
        json personas
    
    get "/personas/:id" $ do
        id <- param "id"
        persona <- lift $ getPersonaAction id
        json persona
    
    post "/personas/" $ do
        data' <- jsonData
        persona <- lift $ createPersonaAction data'
        json (personaId persona)
    
    put "/personas/:id" $ do
        id <- param "id"
        data' <- jsonData
        lift $ updatePersonaAction id data'
    
    delete "/personas/:id" $ do
        id <- param "id"
        lift $ deletePersonaAction id