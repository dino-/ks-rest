-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Handler.PlaceId ( handler )
   where

import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import KS.Data.BSON ( bsonToDoc )
import Web.Scotty ( ActionM, json, param )

import KS.Server.Config
import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   placeId <- param "placeid"

   liftIO $ infoM lname $ "by_placeid received, placeid: " ++ (T.unpack placeId)

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find (select ["place.place_id" =: placeId] "inspections")
         { sort = [ "inspection.date" =: (-1 :: Int) ] }
   json . map bsonToDoc $ ds
