-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspSorted
   ( defaultLimit, handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import           Data.Bson.Generic ( fromBSON )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant
                  ( ServantErr (errBody)
                  , err400
                  )
import           Text.Printf ( printf )

import qualified KS.Data.Document as D
import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname )
import           KS.Rest.Util ( requiredParam, verifyAPIKey )


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


metersToEarthRadians :: Double -> Double
metersToEarthRadians m = (mToKm m) / earthRadiansPerKm
   where
      -- Convert meters to kilometers
      mToKm m' = m' * 0.001

      -- Number of radians on the Earth coordinate system in
      -- a kilometer
      earthRadiansPerKm = 6378.1

      -- Number of radians on the Earth coordinate system in
      -- a mile. Not used, but in case we have a future need.
      --earthRadiansPerMile = 3963.2


parseSortParam :: T.Text -> EitherT ServantErr IO Order
parseSortParam paramValue = do
   ordering <- convertOrdering $ T.take 1 paramValue
   field <- verifyField $ T.drop 1 paramValue
   return $ [ (T.concat ["inspection.", field]) =: ordering ]

   where
      convertOrdering :: T.Text -> EitherT ServantErr IO Int
      convertOrdering "+" = return 1
      convertOrdering "-" = return (-1)
      convertOrdering o   = left $ err400 { errBody = C.concat
         [ "Cannot parse sort parameter because we got sort ordering '"
         , (C.pack . T.unpack $ o)
         , "' but expected '+' or '-'"
         ] }

      validSortFields :: [T.Text]
      validSortFields = ["date", "score"]

      verifyField :: T.Text -> EitherT ServantErr IO T.Text
      verifyField field
         | field `elem` validSortFields = return field
         | otherwise = left $ err400 { errBody = C.concat
            [ "Cannot parse sort parameter because we got sort field '"
            , (C.pack . T.unpack $ field)
            , "' but expected one of "
            , (C.pack . show $ validSortFields)
            ] }
