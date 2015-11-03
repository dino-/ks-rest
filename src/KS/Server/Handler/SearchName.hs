-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Server.Handler.SearchName ( handler )
   where

import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import KS.Data.BSON ( bsonToDoc )
import Web.Scotty ( ActionM, json, param )

import KS.Server.Config
--import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   regex' <- param "regex"
   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find (select ["place.name" =: Regex (regex' :: T.Text) "i"] "inspections")
   json . map bsonToDoc $ ds
