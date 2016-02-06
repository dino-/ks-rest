-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.StatsLatest ( handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Aeson ( Value (Object) )
import           Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr )

import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname )
import           KS.Rest.Util ( requiredParam, verifyAPIKey )


handler
   :: Config -> Pipe
   -> Maybe String -> Maybe T.Text
   -> EitherT ServantErr IO [Value]
handler conf pipe mbKey mbSources = do
   liftIO $ lineM

   let mc = mongoConf conf

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   sources <- (T.split (== ',')) <$> requiredParam "sources" mbSources

   liftIO $ infoM lname
      $ "stats latest by_source received, sources: "
      ++ (show sources)

   ds <- access pipe slaveOk (database mc) $ rest =<<
      find ( select
         [ "doctype" =: ("regional_stats" :: T.Text)
         , "source" =: [ "$in" =: sources ]
         ]
         "regional_data"
         )

   return $ map (Object . toAeson) ds
