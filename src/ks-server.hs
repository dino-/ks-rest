-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Database.MongoDB hiding ( options )
import System.Environment ( getArgs )
import Web.Scotty ( get, scotty, text )

import KS.Server.Config
import qualified KS.Server.Handler.ByLocMostRec as ByLocMostRec
import qualified KS.Server.Handler.SearchName as SearchName
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   initLogging (logPriority config) (logPath config)

   noticeM lname "ks-server started"

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   -- Start the server
   scotty (webServerPort config) $ do
      -- These are the method/route/handler definitions
      get "/ping" $ text "pong\n"
      get "/inspections/search_name" $ SearchName.handler mc pipe
      get "/inspections/by_loc" $ ByLocMostRec.handler mc pipe

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}
