-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Inspections.ByName ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Web.Scotty ( ActionM, json, param )

import KS.Server.Config
import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   liftIO $ lineM

   regex' <- param "regex"

   liftIO $ infoM lname $ "by_name received, regex: " ++ (T.unpack regex')

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find (select ["place.name" =: Regex (regex' :: T.Text) "i"] "inspections")
   json . map toAeson $ ds
