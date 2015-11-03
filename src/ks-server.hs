-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

--import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import KS.Data.BSON ( bsonToDoc )
import System.Environment ( getArgs )
import Web.Scotty ( ActionM, get, json, param, scotty, text )

import KS.Server.Config
import qualified KS.Server.Handler.ByLocMostRec as ByLocMostRec
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
   scotty 3000 $ do
      -- These are the method/route/handler definitions
      get "/hello" $ text "Hello world!"
      get "/inspections/search-name" $ searchName mc pipe
      get "/inspections/by_loc" $ ByLocMostRec.handler mc pipe

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}


-- Request handlers

searchName :: MongoConf -> Pipe -> ActionM ()
searchName mc pipe = do
   regex' <- param "regex"
   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find (select ["place.name" =: Regex (regex' :: T.Text) "i"] "inspections")
   json . map bsonToDoc $ ds
