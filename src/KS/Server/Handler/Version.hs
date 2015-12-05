-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Handler.Version ( handler )
   where

import Data.Aeson.Bson ( toAeson )
import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import Paths_ks_server ( version )
import Web.Scotty ( ActionM, json )

import KS.Server.Log


ksServerVersion, ksAPIVersion :: String

-- This comes from the .cabal file
ksServerVersion = showVersion version

ksAPIVersion = "1.0"


handler :: ActionM ()
handler = do
   liftIO $ lineM

   json . toAeson $
      [ "ks_server_version" =: ksServerVersion
      , "ks_api_version" =: ksAPIVersion
      ]
