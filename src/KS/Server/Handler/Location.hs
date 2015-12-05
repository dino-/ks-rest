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
   liftIO $ lineM

   pt <- parseLngLat <$> param "pt"
   dist <- param "dist" `rescue` (return . const defaultDistance)
   minScore <- param "min_score" `rescue` (return . const defaultMinScore)

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
   let documents = ("results" `at` r) :: [Document]

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length documents

   -- This one returns everything we get from mongo
   --    { results: [], state: {}, ok: 1 }
   --json . toAeson $ r

   -- Just the inspections, no dist ("traditional" format)
   --    [ { _id: .., inspection: .. ... }, ... ]
   --json . map toAeson . map (at "obj") $ documents

   -- The inspections and their distances, side-by-side
   --    [ { obj: { _id: ... }, dis: 799.72633 }, ... ]
   json . map toAeson $ documents

   -- The inspections with dis inserted into the inspection
{-
   json . map toAeson . map combineObjAndDis $ documents


combineObjAndDis :: Document -> Document
combineObjAndDis doc = merge distance inspection
   where
      distance = ["dis" =: (("dis" `at` doc) :: Double)]
      inspection = "obj" `at` doc
-}


parseLngLat :: T.Text -> [Double]
parseLngLat = map (read . T.unpack) . (T.split (== ','))
