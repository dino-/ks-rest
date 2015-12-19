-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Stats.Latest ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
import Web.Scotty ( ActionM, json, param )

import KS.Server.Config
import KS.Server.Log


handler :: MongoConf -> Pipe -> ActionM ()
handler mc pipe = do
   liftIO $ lineM

   sources <- (T.split (== ',')) <$> param "sources"

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

   json . map toAeson $ ds
