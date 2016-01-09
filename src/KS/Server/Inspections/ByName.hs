-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Inspections.ByName ( handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import           Data.Bson.Generic ( fromBSON )
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( options )
import           Servant ( ServantErr (errBody) , err400 )

import qualified KS.Data.Document as D
import           KS.Server.Config ( MongoConf (database) )
import           KS.Server.Log ( infoM, lineM, lname )


handler :: MongoConf -> Pipe -> Maybe T.Text -> EitherT ServantErr IO [D.Document]
handler mc pipe mregex = do
   liftIO $ lineM

   regex' <- maybe
      (left $ err400 { errBody = "Missing required query param: regex" })
      return mregex

   liftIO $ infoM lname $ "by_name received, regex: " ++ (T.unpack regex')

   -- ds :: [Data.Bson.Document]
   ds <- access pipe slaveOk (database mc) $ rest =<<
      find (select ["place.name" =: Regex (regex' :: T.Text) "i"] "inspections")

   return $ catMaybes . map fromBSON $ ds
