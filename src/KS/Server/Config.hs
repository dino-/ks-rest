-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Config
   ( Config (..)
   , loadConfig
   )
   where

import Data.Text as T
import System.FilePath
import System.Log
import TCE.Data.ReadConf ( readConfig )


data Config = Config
   { webServerPort :: Int

   , mongoServerIP :: String

   , mongoUsername :: T.Text
   , mongoPassword :: T.Text

   , mongoDatabase :: T.Text

   , mongoCollection :: T.Text

   , logPriority :: Priority
   , logPath :: FilePath
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig confDir = do
   let confPath = confDir </> "ks-server.conf"
   (either error id . readConfig) `fmap` readFile confPath
