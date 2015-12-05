-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Handler.Source ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Data.Text.Lazy ( fromStrict )
import Database.MongoDB hiding ( options )
import Network.HTTP.Types.Status ( badRequest400 )
import Text.Printf ( printf )
import Web.Scotty ( ActionM, json, param, rescue, status, text )

import KS.Server.Config
import KS.Server.Log


defaultLimit :: Limit
defaultLimit = 100


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   liftIO $ lineM

   criteria <- param "criteria"
   sources <- (T.split (== ',')) <$> param "sources"
   limit' <- param "limit" `rescue` (return . const defaultLimit)

   liftIO $ infoM lname
      $ printf "by_source received, criteria: %s, sources: %s, limit: %d"
      (T.unpack criteria) (show sources) (limit' :: Limit)

   let critEval = case (criteria :: T.Text) of
         "high"   -> Right ([ "inspection.score" =: (-1 :: Int) ], "recent_inspections")
         "low"    -> Right ([ "inspection.score" =: ( 1 :: Int) ], "recent_inspections")
         "latest" -> Right ([ "inspection.date"  =: (-1 :: Int) ], "inspections")
         _        -> Left  criteria

   either badCriteriaFail (queryAndRespond pipe mc sources limit') critEval


queryAndRespond :: (Val v) => Pipe -> MongoConf -> v -> Limit -> (Order, Collection) -> ActionM ()
queryAndRespond pipe mc sources limit' (sort', coll') = do
   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find ( select
         [ "inspection.inspection_source" =: [ "$in" =: sources ] ] coll'
         ) { sort = sort' , limit = limit' }

   json . map toAeson $ ds


badCriteriaFail :: T.Text -> ActionM ()
badCriteriaFail criteria = do
   let errMsg = "Invalid criteria: " ++ (T.unpack criteria)
   liftIO $ warningM lname errMsg

   status badRequest400
   text . fromStrict . T.concat $ [T.pack errMsg, "\n"]
