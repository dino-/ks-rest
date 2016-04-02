-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspRecentNear
   ( defaultMinScore, handler )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Except ( ExceptT )
import Data.Aeson ( Value (Object) )
import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( Value, options )
import Servant ( ServantErr )
import Text.Printf ( printf )

import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Types ( ByLocResults (..) )
import KS.Rest.Util ( coll_inspections_recent, requiredParam, verifyAPIKey )


defaultMinScore :: Double
defaultMinScore = 0.0


handler
   :: Config -> Pipe
   -> Maybe String -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
   -> ExceptT ServantErr IO ByLocResults
handler conf pipe mbKey mbLat mbLng mbDist mbMinScore = do
   liftIO $ lineM

   let mc = mongoConf conf

   _              <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   lat            <- requiredParam "lat" mbLat
   lng            <- requiredParam "lng" mbLng
   dist           <- requiredParam "dist" mbDist
   let minScore   =  maybe defaultMinScore id mbMinScore

   liftIO $ infoM lname
      $ printf "search by loc received, lat: %f, lng %f, dist: %f, min_score: %f"
      lat lng dist minScore

   r <- access pipe slaveOk (database mc) $ runCommand (
      [ "geoNear" =: coll_inspections_recent
      , "near" =:
         [ "type" =: ("Point" :: T.Text)
         , "coordinates" =: [ lng, lat ]
         ]
      , "spherical" =: True
      , "limit" =: (5000 :: Int)  -- FIXME Do we need this?
      , "maxDistance" =: dist
      , "query" =: [ "inspection.score" =: [ "$gte" =: minScore ] ]
      ])

   -- Stripping off the stats portion
   let bsonDocs = ("results" `at` r) :: [Document]

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length bsonDocs

   -- The inspections and their distances, side-by-side
   --    [ { obj: { _id: ... }, dis: 799.72633 }, ... ]
   return $ ByLocResults $ map (Object . toAeson) bsonDocs
