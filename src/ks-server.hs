-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ( (&&&) )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import KS.Data.BSON ( bsonToDoc )
import System.Environment ( getArgs )
import Web.Scotty ( ActionM, get, json, param, rescue, scotty, text )

import KS.Server.Config
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   initLogging (logPriority config) (logPath config)

   noticeM lname "ks-server started"

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   -- Start the server
   scotty 3000 $ do
      -- These are the method/route/handler definitions
      get "/hello" $ text "Hello world!"
      get "/inspections/search-name" $ searchName mc pipe
      get "/inspections/by_loc" $ locMostRec mc pipe

   {- These never execute, is that bad? Can do something threaded if necessary

   putStrLn "Server shutting down..."
   close pipe
   -}


-- Request handlers

searchName :: MongoConf -> Pipe -> ActionM ()
searchName mc pipe = do
   regex' <- param "regex"
   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find (select ["place.name" =: Regex (regex' :: T.Text) "i"] "inspections")
   json . map bsonToDoc $ ds


locMostRec :: MongoConf -> Pipe -> ActionM ()
locMostRec mc pipe = do
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

   let pl = [geoNear, dateSort, group', sort']

   ds <- access pipe slaveOk (database mc) $ aggregate "inspections" pl
   liftIO $ debugM lname $ "count = " ++ (show $ length ds)

   json . map toAeson $ ds


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
