-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Handler.Location ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Text.Printf ( printf )
import Web.Scotty ( ActionM, json, param, rescue )

import KS.Server.Config
import KS.Server.Log


-- Default query distance in meters
defaultDistance :: Double
defaultDistance = 800


defaultMinScore :: Double
defaultMinScore = 0.0


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   pt <- parseLngLat <$> param "pt"
   dist <- param "dist" `rescue` (return . const defaultDistance)
   minScore <- param "min_score" `rescue` (return . const defaultMinScore)

   liftIO $ infoM lname $ printf "by_loc received, pt: %s, dist: %f"
      (show pt) dist

   ds <- access pipe slaveOk (database mc) $ runCommand (
      [ "geoNear" =: ("inspections" :: T.Text)
      , "near" =:
         [ "type" =: ("Point" :: T.Text)
         , "coordinates" =: pt
         ]
      , "spherical" =: True
      , "limit" =: (5000 :: Int)
      , "maxDistance" =: dist
      , "query" =: [ "inspection.score" =: [ "$gte" =: minScore ] ]
      ])

   -- This one returns everything we get from mongo
   --json . toAeson $ ds

   -- Just the inspections
   --json . map toAeson . map (at "obj") . (at "results") $ ds

   -- The inspections and their distances
   json . map toAeson . (at "results") $ ds


parseLngLat :: T.Text -> [Double]
parseLngLat = map (read . T.unpack) . (T.split (== ','))
