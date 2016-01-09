-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Version ( handler )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Either ( EitherT )
import Data.Aeson ( Value, (.=), object )
import Data.Version ( showVersion )
import Paths_ks_server ( version )
import Servant ( ServantErr )

import KS.Server.Log ( lineM )


ksServerVersion, ksAPIVersion :: String

-- This comes from the .cabal file
ksServerVersion = showVersion version

ksAPIVersion = "1.0"


handler :: EitherT ServantErr IO Value
handler = do
   liftIO $ lineM

   return $ object
      [ "ks_server_version" .= ksServerVersion
      , "ks_api_version" .= ksAPIVersion
      ]
