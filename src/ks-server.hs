-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import Paths_ks_server ( version )
import System.Environment ( getArgs )
import Web.Scotty ( get, middleware, scotty )

import KS.Server.Config
import qualified KS.Server.Handler.Location as Location
import qualified KS.Server.Handler.Name as Name
import qualified KS.Server.Handler.PlaceId as PlaceId
import qualified KS.Server.Handler.Source as Source
import qualified KS.Server.Handler.Stats.Latest as Stats_Latest
import qualified KS.Server.Handler.Version as Version
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   logger <- initLogging (logPriority config) (logPath config)

   lineM
   noticeM lname $ "ks-server version " ++ (showVersion version) ++ " started"

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)


   -- Start the server
   scotty (webServerPort config) $ do
      middleware logger

      -- Method/route/handler definitions
      --post "/inspections"                       $ Create.handler mc pipe
      get  "/inspections/by_loc"                $ Location.handler mc pipe
      get  "/inspections/by_name"               $ Name.handler mc pipe
      get  "/inspections/by_placeid/:placeid"   $ PlaceId.handler mc pipe
      get  "/inspections/by_source/:criteria"   $ Source.handler mc pipe
      get  "/stats/latest/by_source"            $ Stats_Latest.handler mc pipe
      get  "/version"                           $ Version.handler

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}
