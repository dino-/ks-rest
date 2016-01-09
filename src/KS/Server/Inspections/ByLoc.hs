-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Inspections.ByLoc
   ( defaultDistance, defaultMinScore, handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import           Data.Aeson ( Value (Object) )
import           Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr (errBody) , err400 )
import           Text.Printf ( printf )

import           KS.Server.Config ( MongoConf (database) )
import           KS.Server.Log ( infoM, lineM, lname )
import           KS.Server.Types ( ByLocResults (..) )


-- Default query distance in meters
defaultDistance :: Double
defaultDistance = 800


defaultMinScore :: Double
defaultMinScore = 0.0


handler
   :: MongoConf -> Pipe
   -> Maybe T.Text -> Maybe Double -> Maybe Double
   -> EitherT ServantErr IO ByLocResults
handler mc pipe mbPt mbDist mbMinScore = do
   liftIO $ lineM

   pt <- maybe
      (left $ err400 { errBody = "Missing required query param: pt" })
      (return . parseLngLat) mbPt
   let dist = maybe defaultDistance id mbDist
   let minScore = maybe defaultMinScore id mbMinScore

   liftIO $ infoM lname
      $ printf "by_loc received, pt: %s, dist: %f, minScore: %f"
      (show pt) dist minScore

   r <- access pipe slaveOk (database mc) $ runCommand (
      [ "geoNear" =: ("recent_inspections" :: T.Text)
      , "near" =:
         [ "type" =: ("Point" :: T.Text)
         , "coordinates" =: pt
         ]
      , "spherical" =: True
      , "limit" =: (5000 :: Int)
      , "maxDistance" =: dist
      , "query" =: [ "inspection.score" =: [ "$gte" =: minScore ] ]
      ])

   -- Stripping off the stats portion
   let bsonDocs = ("results" `at` r) :: [Document]

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length bsonDocs

   -- The inspections and their distances, side-by-side
   --    [ { obj: { _id: ... }, dis: 799.72633 }, ... ]
   return $ ByLocResults $ map (Object . toAeson) bsonDocs


parseLngLat :: T.Text -> [Double]
parseLngLat = map (read . T.unpack) . (T.split (== ','))
