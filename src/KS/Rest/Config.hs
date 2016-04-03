-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Config
   ( Config (..)
   , MongoConf (..)
   , loadConfig
   )
   where

import qualified Data.Text as T
import System.FilePath ( (</>) )
import System.Log ( Priority )
import TCE.Data.ReadConf ( readConfig )

import KS.Rest.APIKey ( APIKeys )


data Config = Config
   { webServerPort :: Int
   , mongoConf :: MongoConf
   , logPriority :: Priority
   , logPath :: FilePath
   , apiKeys :: APIKeys
   }
   deriving (Read, Show)


data MongoConf = MongoConf
   { ip :: String
   , username :: T.Text
   , password :: T.Text
   , database :: T.Text
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig confDir = do
   let confPath = confDir </> "ks-rest.conf"
   (either error id . readConfig) `fmap` readFile confPath
