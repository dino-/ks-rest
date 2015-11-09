-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Handler.Source ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Text.Printf ( printf )
import Web.Scotty ( ActionM, json, param, rescue )

import KS.Server.Config
import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   criteria <- param "criteria"
   sources <- (T.split (== ',')) <$> param "sources"
   limit' <- param "limit" `rescue` (return . const 25)

   liftIO $ infoM lname
      $ printf "by_source received, criteria: %s, sources: %s, limit: %d"
      (T.unpack criteria) (show sources) limit'

   let (sort', coll') = case (criteria :: T.Text) of
         "high"   -> ([ "inspection.score" =: (-1 :: Int) ], "recent_inspections")
         "low"    -> ([ "inspection.score" =: ( 1 :: Int) ], "recent_inspections")
         "latest" -> ([ "inspection.date"  =: (-1 :: Int) ], "inspections")
         _        -> undefined

   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find ( select
         [ "inspection.inspection_source" =: [ "$in" =: sources ] ] coll'
         ) { sort = sort' , limit = limit' }

   json . map toAeson $ ds
