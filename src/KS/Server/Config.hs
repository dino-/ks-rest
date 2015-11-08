-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Config
   ( Config (..)
   , MongoConf (..)
   , loadConfig
   )
   where

import Data.Text as T
import System.FilePath
import System.Log
import TCE.Data.ReadConf ( readConfig )


data Config = Config
   { webServerPort :: Int
   , mongoConf :: MongoConf
   , logPriority :: Priority
   , logPath :: FilePath
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
   let confPath = confDir </> "ks-server.conf"
   (either error id . readConfig) `fmap` readFile confPath
