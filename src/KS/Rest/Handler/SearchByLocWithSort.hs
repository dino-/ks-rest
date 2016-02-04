-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.SearchByLocWithSort
   ( defaultLimit, handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Bson.Generic ( fromBSON )
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr )
import           Text.Printf ( printf )

import qualified KS.Data.Document as D
import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname )
import           KS.Rest.Util ( metersToEarthRadians, parseSortParam
                  , requiredParam, verifyAPIKey )


defaultLimit :: Limit
defaultLimit = 100


handler
   :: Config -> Pipe -> Collection
   -> Maybe String -> Maybe Double -> Maybe Double -> Maybe Double
   -> Maybe Limit -> Maybe T.Text
   -> EitherT ServantErr IO [D.Document]
handler
   conf pipe collection
   mbKey mbLat mbLng mbDist
   mbLimit mbSort = do

   liftIO $ lineM

   let mc = mongoConf conf

   _           <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   lat         <- requiredParam "lat" mbLat
   lng         <- requiredParam "lng" mbLng
   dist        <- metersToEarthRadians <$> requiredParam "dist" mbDist
   let limit'  =  maybe defaultLimit id mbLimit
   sort'       <- requiredParam "sort" mbSort >>= parseSortParam

   liftIO $ infoM lname
      -- $ printf "inspections all latest received, lat: %f, lng: %f, dist: (%s), limit: %d, sort: %s"
      $ printf "%s search received, lat: %f, lng: %f, dist: (%s), limit: %d, sort: %s"
      (show collection) lat lng (show mbDist) (limit' :: Limit) (show sort')

   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find ( select
         [ "place.location" =:
            [ "$geoWithin" =:
               [ "$centerSphere" =: Array
                  [ Array [Float lng, Float lat]
                  , Float dist
                  ]
               ]
            ]
         --, "blah.date" =: ???
         ] collection
         ) { sort = sort' , limit = limit' }

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
