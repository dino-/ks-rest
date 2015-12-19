-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import Paths_ks_server ( version )
import System.Environment ( getArgs )
import Web.Scotty ( get, middleware, scotty )

import KS.Server.Config
import qualified KS.Server.Inspections.ByLoc
import qualified KS.Server.Inspections.ByName
import qualified KS.Server.Inspections.ByPlaceID
import qualified KS.Server.Inspections.BySource
import qualified KS.Server.Stats.Latest
import qualified KS.Server.Version
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
      --post "/v1.0/inspections"                       $ KS.Server.Inspections.Create.handler mc pipe
      get  "/v1.0/inspections/by_loc"                $ KS.Server.Inspections.ByLoc.handler mc pipe
      get  "/v1.0/inspections/by_name"               $ KS.Server.Inspections.ByName.handler mc pipe
      get  "/v1.0/inspections/by_placeid/:placeid"   $ KS.Server.Inspections.ByPlaceID.handler mc pipe
      get  "/v1.0/inspections/by_source/:criteria"   $ KS.Server.Inspections.BySource.handler mc pipe
      get  "/v1.0/stats/latest/by_source"            $ KS.Server.Stats.Latest.handler mc pipe
      get  "/v1.0/version"                           $ KS.Server.Version.handler

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}
