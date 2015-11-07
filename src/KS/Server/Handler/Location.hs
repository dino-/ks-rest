-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Server.Handler.Location ( handler )
   where

import Control.Arrow ( (&&&) )
import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Web.Scotty ( ActionM, json, param, rescue )

import KS.Server.Config
--import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   pt <- parseLngLat <$> param "pt"
   --liftIO $ debugM lname $ "pt = " ++ show pt
   dist <- param "dist" `rescue` (return . const 800)
   --liftIO $ debugM lname $ "dist = " ++ show dist

   let geoNear =
         [ "$geoNear" =:
            [ "near" =:
               [ "type" =: ("Point" :: T.Text)
               , "coordinates" =: pt
               ]
            , "distanceField" =: ("dist.calculated" :: T.Text)
            , "maxDistance" =: (dist :: Double)
            , "includeLocs" =: ("place.location" :: T.Text)
            , "num" =: (5000 :: Double)
            , "spherical" =: True
            ]
        ]

   let dateSort = ["$sort" =: ["inspection.date" =: (-1 :: Int)]]

   let group' =
         [ "$group" =:
            [ "_id" =: ("$place.place_id" :: T.Text)
            , "last_inspection" =:
               [ "$first" =:
                  [ "_id" =: ("$_id" :: T.Text)
                  , "doctype" =: ("$doctype" :: String)
                  , "inspection" =: ("$inspection" :: T.Text)
                  , "place" =: ("$place" :: T.Text)
                  ]
               ]
            , "dist" =: ["$max" =: ("$dist" :: T.Text)]
            ]
         ]

   let sort' = ["$sort" =: ["dist" =: (1 :: Int)]]

   let pipeline = [geoNear, dateSort, group', sort']

   ds <- access pipe slaveOk (database mc) $ aggregate "inspections" pipeline

   -- This one will includ the (odd) _id and the dist from the group
   -- We'll need this later
   --json . map toAeson $ ds

   -- This one will return only the traditional inspection record
   json . map toAeson . map (at "last_inspection") $ ds


{- Parse point data from query strings

   from this: "75.1,-36.2"  (Text, similar to String)
   to this:   [75.1, -36.2] ([Double], a List of Double)
-}
parseLngLat :: T.Text -> [Double]
parseLngLat t = [toDouble lng, toDouble lat]
   where
      -- Split on the comma, not including the comma
      -- T.Text -> (T.Text, T.Text)
      (lng, lat) = (T.takeWhile p &&& T.tail . T.dropWhile p) t
      p = (/= ',')

      toDouble :: T.Text -> Double
      toDouble = read . T.unpack
