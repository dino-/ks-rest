-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspAllPlaceIDCap ( handler )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Except ( ExceptT )
import Data.Bson.Generic ( fromBSON )
import Data.Maybe ( catMaybes )
import Data.Pool ( Pool, withResource )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Servant ( ServantErr )
import Text.Printf ( printf )

import qualified KS.Data.Document as D
import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Util ( coll_inspections_all, requiredParam, verifyAPIKey )


handler :: Config -> Pool Pipe -> T.Text -> Maybe String
   -> ExceptT ServantErr IO [D.Document]
handler conf pool placeId mbKey = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead

   liftIO $ infoM lname $ "by_placeid received, placeid: " ++ (T.unpack placeId)

   ds <- withResource pool (\pipe ->
      access pipe slaveOk (database mc) $ rest =<<
         find (select ["place.place_id" =: placeId] coll_inspections_all)
            { sort = [ "inspection.date" =: (-1 :: Int) ] }
      )

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
