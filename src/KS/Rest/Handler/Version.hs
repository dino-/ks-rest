-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.Version ( handler )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Either ( EitherT )
import Data.Aeson ( Value, (.=), object )
import Data.Version ( showVersion )
import Paths_ks_rest ( version )
import Servant ( ServantErr )

import KS.Rest.Log ( lineM )


ksRestServerVersion, ksRestAPIVersion :: String

-- This comes from the .cabal file
ksRestServerVersion = showVersion version

ksRestAPIVersion = "1.0"


handler :: EitherT ServantErr IO Value
handler = do
   liftIO $ lineM

   return $ object
      [ "ks_rest_server_version" .= ksRestServerVersion
      , "ks_rest_api_version" .= ksRestAPIVersion
      ]
