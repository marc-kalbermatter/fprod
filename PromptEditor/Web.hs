{-# LANGUAGE OverloadedStrings #-}

module PromptEditor.Web where

import PromptEditor.ChatGPT
import PromptEditor.Environment
import PromptEditor.Types
import PromptEditor.Application
import Database.SQLite.Simple (withConnection, Connection)
import Control.Applicative ()
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Data.Aeson ( eitherDecode, ToJSON )
import Web.Scotty.Trans
import Control.Monad.Reader ( ReaderT(runReaderT), MonadTrans (lift) )
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Cors ( simpleCors )
import Network.Wai.Middleware.Static ( (>->), addBase, noDots, staticPolicy, unsafeStaticPolicy )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Web.Scotty.Internal.Types (RoutePattern(Capture))
import Network.Wai (Middleware)

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
            expertRepository = DB.createExpertRepository conn
            stepsRepostiroty = DB.createStepsRepository conn
            avoidRepostiroty = DB.createAvoidRepository conn
            formatRepostiroty = DB.createFormatRepository conn
            chatGPTUrl = apiUrl $ chatGPT config
            chatGPTKey = apiKey $ chatGPT config
            sendRequest = makeRequest chatGPTUrl chatGPTKey
            env = Env
                    personaRepository
                    goalRepository
                    expertRepository
                    stepsRepostiroty
                    avoidRepostiroty
                    formatRepostiroty
                    sendRequest
            
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
    middleware corsMiddleware

    get "/" $ file "ElmMain/src/index.html"

    crudEndpoints "personas" envPersonaRepository

    crudEndpoints "goals" envGoalRepository
    
    crudEndpoints "experts" envExpertRepository

    crudEndpoints "steps" envStepsRepository

    crudEndpoints "avoids" envAvoidRepository

    crudEndpoints "formats" envFormatRepository

    options (regex "/*") $ do text "Success"

routePattern :: String -> RoutePattern
routePattern = Capture . T.pack

crudEndpoints :: ToJSON a => String -> (Env -> Repository a) -> ScottyT T.Text App ()
crudEndpoints typeName env = do

    get (routePattern ("/" ++ typeName)) $ do
        expertIn <- lift $ getAllAction env
        json expertIn
    
    get (routePattern ("/" ++ typeName ++ "/:id")) $ do
        id <- param "id"
        expertIn <- lift $ getAction env id
        json expertIn
    
    post (routePattern ("/" ++ typeName)) $ do
        data' <- jsonData
        dataWithId <- lift $ createAction env data'
        json dataWithId

    put (routePattern ("/" ++ typeName ++ "/:id")) $ do
        id <- param "id"
        data' <- jsonData
        lift $ updateAction env id data'
    
    delete (routePattern ("/" ++ typeName ++ "/:id")) $ do
        id <- param "id"
        lift $ deleteAction env id

corsMiddleware :: Middleware
corsMiddleware = addHeaders
    [
        ("Access-Control-Allow-Origin", "*"),
        ("Access-Control-Allow-Headers", "*"),
        ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE")
    ]