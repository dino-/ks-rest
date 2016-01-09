-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Inspections.ByPlaceID ( handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Bson.Generic ( fromBSON )
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( options )
import           Servant ( ServantErr )
import           Text.Printf ( printf )

import qualified KS.Data.Document as D
import           KS.Server.Config ( MongoConf (database) )
import           KS.Server.Log ( infoM, lineM, lname )


handler :: MongoConf -> Pipe -> T.Text -> EitherT ServantErr IO [D.Document]
handler mc pipe placeId = do
   liftIO $ lineM

   liftIO $ infoM lname $ "by_placeid received, placeid: " ++ (T.unpack placeId)

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find (select ["place.place_id" =: placeId] "inspections")
         { sort = [ "inspection.date" =: (-1 :: Int) ] }

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
