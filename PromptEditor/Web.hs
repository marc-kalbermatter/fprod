{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.Web where

import PromptEditor.Types
import qualified PromptEditor.DB as DB
import           Database.SQLite.Simple (withConnection, Connection)
import           Control.Applicative
import           Control.Monad (mzero)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import           Data.Aeson hiding (json)
import           Web.Scotty.Trans
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           System.Exit
import           System.IO

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