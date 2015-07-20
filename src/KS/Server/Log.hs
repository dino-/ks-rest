-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Log
   ( initLogging, lname

   -- Re-exported from System.Log
   , debugM, infoM, noticeM, warningM, errorM , criticalM, alertM, emergencyM
   )
   where

import System.Log.Formatter ( simpleLogFormatter )
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( fileHandler )
import System.Log.Logger


lname :: String
lname = rootLoggerName


initLogging :: Priority -> FilePath -> IO ()
initLogging priority logFilePath = do
   -- Remove the root logger's default handler that writes every
   -- message to stderr!
   updateGlobalLogger lname removeHandler
   updateGlobalLogger lname $ setLevel priority

   -- A file handler with timestamping
   (flip setFormatter $ simpleLogFormatter "[$time : $prio] $msg")
      <$> fileHandler logFilePath DEBUG
      >>= updateGlobalLogger lname . addHandler
