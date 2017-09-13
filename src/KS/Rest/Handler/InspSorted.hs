-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspSorted
   ( defaultLimit, handler )
   where

import Control.Monad.Except ( throwError )
import Control.Monad.Trans ( liftIO )
import Data.Bson.Generic ( fromBSON )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe ( catMaybes )
import Data.Pool ( Pool, withResource )
import qualified Data.Text as T
import Database.MongoDB hiding ( Value, options )
import Servant
   ( Handler
   , ServantErr (errBody)
   , err400
   )
import Text.Printf ( printf )

import qualified KS.Data.Document as D
import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Util ( requiredParam, verifyAPIKey )


defaultLimit :: Limit
defaultLimit = 100


handler
   :: Config -> Pool Pipe -> Collection
   -> Maybe String -> Maybe Double -> Maybe Double -> Maybe Double
   -> Maybe Limit -> Maybe T.Text
   -> Handler [D.Document]
handler
   conf pool collection
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
      $ printf "%s search received, lat: %f, lng: %f, dist: (%s), limit: %d, sort: %s"
      (show collection) lat lng (show mbDist) (limit' :: Limit) (show sort')

   ds <- withResource pool (\pipe ->
      access pipe slaveOk (database mc) $ do
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
      )

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds


metersToEarthRadians :: Double -> Double
metersToEarthRadians m = (mToKm m) / kmPerEarthRadian
   where
      -- Convert meters to kilometers
      mToKm m' = m' * 0.001

      -- The Earth coordinate system being used by MongoDB's
      -- geoWithin indexing is: the circumference of the Earth measures
      -- 2pi radians
      -- So, with that in mind:

      -- Number of kilometers on the Earth coordinate system in a
      -- single radian
      kmPerEarthRadian = 6378.1

      -- Number of miles on the Earth coordinate system in a
      -- single radian
      -- milesPerEarthRadian = 3963.2


parseSortParam :: T.Text -> Handler Order
parseSortParam paramValue = do
   ordering <- convertOrdering $ T.take 1 paramValue
   field <- verifyField $ T.drop 1 paramValue
   return $ [ (T.concat ["inspection.", field]) =: ordering ]

   where
      convertOrdering :: T.Text -> Handler Int
      convertOrdering "+" = return 1
      convertOrdering "-" = return (-1)
      convertOrdering o   = throwError $ err400 { errBody = C.concat
         [ "Cannot parse sort parameter because we got sort ordering '"
         , (C.pack . T.unpack $ o)
         , "' but expected '+' or '-'"
         ] }

      validSortFields :: [T.Text]
      validSortFields = ["date", "score"]

      verifyField :: T.Text -> Handler T.Text
      verifyField field
         | field `elem` validSortFields = return field
         | otherwise = throwError $ err400 { errBody = C.concat
            [ "Cannot parse sort parameter because we got sort field '"
            , (C.pack . T.unpack $ field)
            , "' but expected one of "
            , (C.pack . show $ validSortFields)
            ] }
