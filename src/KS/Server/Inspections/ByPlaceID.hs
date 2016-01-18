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
import           KS.Server.APIKey ( akRead )
import           KS.Server.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Server.Log ( infoM, lineM, lname )
import           KS.Server.Util ( requiredParam, verifyAPIKey )


handler :: Config -> Pipe -> T.Text -> Maybe String
   -> EitherT ServantErr IO [D.Document]
handler conf pipe placeId mbKey = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead

   liftIO $ infoM lname $ "by_placeid received, placeid: " ++ (T.unpack placeId)

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find (select ["place.place_id" =: placeId] "inspections")
         { sort = [ "inspection.date" =: (-1 :: Int) ] }

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
