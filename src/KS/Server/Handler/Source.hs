-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Server.Handler.Source ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Web.Scotty ( ActionM, json, param, rescue, text )

import KS.Server.Config
--import KS.Server.Log


{-
handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   criteria <- param "criteria"
   sources <- (T.split (== ',')) <$> param "sources"
   limit' <- param "limit" `rescue` (return . const 25)

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

   let match = ["$match" =: ["inspection.inspection_source" =: ["$in" =: sources]]]

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

   let scoreSort = ["$sort" =: ["dist" =: (1 :: Int)]]  -- high/low

   let limit'' = ["$limit" =: limit']

   let pipeline = [match, dateSort, group', scoreSort, limit'']

   ds <- access pipe slaveOk (database mc) $ aggregate "inspections" pipeline

   -- This one will includ the (odd) _id and the dist from the group
   -- We'll need this later
   --json . map toAeson $ ds

   -- This one will return only the traditional inspection record
   json . map toAeson . map (at "last_inspection") $ ds
-}

handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   criteria <- param "criteria"
   sources <- (T.split (== ',')) <$> param "sources"
   limit' <- param "limit" `rescue` (return . const 25)

   let sort' = case (criteria :: T.Text) of
         "high"   -> [ "inspection.score" =: (-1 :: Int) ]
         "low"    -> [ "inspection.score" =: ( 1 :: Int) ]
         "latest" -> [ "inspection.date"  =: (-1 :: Int) ]
         _        -> undefined

   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find ( select
         [ "inspection.inspection_source" =: [ "$in" =: sources ] ] "inspections" 
         ) { sort = sort' , limit = limit' }

   json . map toAeson $ ds
