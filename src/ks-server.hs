-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import Network.Wai.Middleware.Gzip ( def )
import Network.Wai.Middleware.RequestLogger ( OutputFormat (..), IPAddrSource (..), mkRequestLogger, outputFormat )
import Paths_ks_server ( version )
import System.Environment ( getArgs )
import Web.Scotty ( get, middleware, scotty, text )

import KS.Server.Config
import qualified KS.Server.Handler.Location as Location
import qualified KS.Server.Handler.Name as Name
import qualified KS.Server.Handler.PlaceId as PlaceId
import qualified KS.Server.Handler.Source as Source
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   initLogging (logPriority config) (logPath config)

   noticeM lname $ "ks-server version " ++ (showVersion version) ++ " started"

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   reqLogger <- liftIO $ mkRequestLogger def { outputFormat = Apache FromFallback }
   --let reqLogger = logStdoutDev

   -- Start the server
   scotty (webServerPort config) $ do
      --log requests to console
      middleware reqLogger

      -- Method/route/handler definitions
      --post "/inspections"                       $ Create.handler mc pipe
      get  "/inspections/by_loc"                $ Location.handler mc pipe
      get  "/inspections/by_name"               $ Name.handler mc pipe
      get  "/inspections/by_placeid/:placeid"   $ PlaceId.handler mc pipe
      get  "/inspections/by_source/:criteria"   $ Source.handler mc pipe
      get  "/ping"                              $ text "pong\n"

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}


{-
import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Control.Monad.IO.Class
import Data.Monoid (mconcat)
import Data.Default

main = scotty 3000 $ do
    --log requests to console
    logger <- liftIO $ mkRequestLogger def { outputFormat = Apache FromHeader }
    middleware logger

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}
