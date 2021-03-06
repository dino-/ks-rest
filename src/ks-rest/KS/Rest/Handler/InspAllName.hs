-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspAllName ( handler )
   where

import Control.Monad.Trans ( liftIO )
import Data.Bson.Generic ( fromBSON )
import Data.Maybe ( catMaybes )
import Data.Pool ( Pool, withResource )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Servant ( Handler )
import Text.Printf ( printf )

import qualified KS.Data.Document as D
import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Util ( coll_inspections_all, requiredParam, verifyAPIKey )


handler :: Config -> Pool Pipe -> Maybe String -> Maybe T.Text
   -> Handler [D.Document]
handler conf pool mbKey mbRegex = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   regex' <- requiredParam "regex" mbRegex

   liftIO $ infoM lname $ "by_name received, regex: " ++ (T.unpack regex')

   -- ds :: [Data.Bson.Document]
   ds <- withResource pool (\pipe ->
      access pipe slaveOk (database mc) $ rest =<<
         find (select ["place.name" =: Regex (regex' :: T.Text) "i"]
            coll_inspections_all)
      )

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
