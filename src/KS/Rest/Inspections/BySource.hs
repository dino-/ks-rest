-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Inspections.BySource
   ( defaultLimit, handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import           Data.Bson.Generic ( fromBSON )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr (errBody) , err400 )
import           Text.Printf ( printf )

import qualified KS.Data.Document as D
import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname, warningM )
import           KS.Rest.Util ( requiredParam, verifyAPIKey )


defaultLimit :: Limit
defaultLimit = 100


handler
   :: Config -> Pipe
   -> T.Text -> Maybe String -> Maybe T.Text -> Maybe Limit
   -> EitherT ServantErr IO [D.Document]
handler conf pipe criteria mbKey mbSources mbLimit = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   sources <- (T.split (== ',')) <$> requiredParam "sources" mbSources
   let limit' = maybe defaultLimit id mbLimit

   liftIO $ infoM lname
      $ printf "by_source received, criteria: %s, sources: %s, limit: %d"
      (T.unpack criteria) (show sources) (limit' :: Limit)

   let critEval = case (criteria :: T.Text) of
         "high"   -> Right ([ "inspection.score" =: (-1 :: Int) ], "recent_inspections")
         "low"    -> Right ([ "inspection.score" =: ( 1 :: Int) ], "recent_inspections")
         "latest" -> Right ([ "inspection.date"  =: (-1 :: Int) ], "inspections")
         _        -> Left  criteria

   either badCriteriaFail (queryAndRespond pipe mc sources limit') critEval


queryAndRespond
   :: (Val v)
   => Pipe -> MongoConf
   -> v -> Limit -> (Order, Collection)
   -> EitherT ServantErr IO [D.Document]
queryAndRespond pipe mc sources limit' (sort', coll') = do
   ds <- access pipe slaveOk (database mc) $ do
      rest =<< find ( select
         [ "inspection.inspection_source" =: [ "$in" =: sources ] ] coll'
         ) { sort = sort' , limit = limit' }

   return $ catMaybes . map fromBSON $ ds


badCriteriaFail :: T.Text -> EitherT ServantErr IO [D.Document]
badCriteriaFail criteria = do
   let errMsg = "Invalid criteria: " ++ (T.unpack criteria)
   liftIO $ warningM lname errMsg

   left $ err400 { errBody = C.pack errMsg }