-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.StatsLatest
   ( handlerBySource
   , handlerRecentNear
   )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Aeson ( Value (Object) )
import           Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr )
import           Text.Printf ( printf )

import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname )
import           KS.Rest.Util
                  ( coll_stats_recent, requiredParam, verifyAPIKey )


handlerBySource
   :: Config -> Pipe
   -> Maybe String -> Maybe T.Text
   -> EitherT ServantErr IO [Value]
handlerBySource conf pipe mbKey mbSources = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   sources <- (T.split (== ',')) <$> requiredParam "sources" mbSources

   liftIO $ infoM lname
      $ "stats latest by_source received, sources: "
      ++ (show sources)

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find ( select
         [ "doctype" =: ("regional_stats" :: T.Text)
         , "source" =: [ "$in" =: sources ]
         ]
         coll_stats_recent
         )

   return $ map (Object . toAeson) ds


handlerRecentNear
   :: Config -> Pipe
   -> Maybe String -> Maybe Double -> Maybe Double -> Maybe Double
   -> EitherT ServantErr IO [Value]
handlerRecentNear conf pipe mbKey mbLat mbLng mbDist = do
   liftIO $ lineM

   let mc = mongoConf conf

   _              <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   lat            <- requiredParam "lat" mbLat
   lng            <- requiredParam "lng" mbLng
   dist           <- requiredParam "dist" mbDist

   liftIO $ infoM lname
      $ printf "stats recent near received, lat: %f, lng %f, dist: %f"
      lat lng dist

   r <- access pipe slaveOk (database mc) $ runCommand (
      [ "geoNear" =: coll_stats_recent
      , "near" =:
         [ "type" =: ("Point" :: T.Text)
         , "coordinates" =: [ lng, lat ]
         ]
      , "spherical" =: True
      , "limit" =: (5000 :: Int)  -- FIXME Do we need this?
      , "maxDistance" =: dist
      -- , "query" =: [ "inspection.score" =: [ "$gte" =: minScore ] ]
      ])

   -- Stripping off the stats portion
   let bsonDocs = ("results" `at` r) :: [Document]

   liftIO $ infoM lname $ printf "Retrieved %d stats records" $ length bsonDocs

   -- A list of the stats documents that were retrieved
   --    [ { _id: ... }, ... ]
   return $ map (Object . toAeson . ("obj" `at`)) bsonDocs
