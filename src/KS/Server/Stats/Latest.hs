-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Stats.Latest ( handler )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Aeson ( Value (Object) )
import           Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import           Database.MongoDB hiding ( Value, options )
import           Servant ( ServantErr )

import           KS.Server.Config ( MongoConf (database) )
import           KS.Server.Log ( infoM, lineM, lname )
import           KS.Server.Util ( requiredParam )


handler
   :: MongoConf -> Pipe
   -> Maybe T.Text
   -> EitherT ServantErr IO [Value]
handler mc pipe mbSources = do
   liftIO $ lineM

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
