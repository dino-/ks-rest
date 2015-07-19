-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Log
   ( initLogging, lname

   -- Re-exported from System.Log
   , Priority (..), debugM, infoM, noticeM, warningM, errorM
   , criticalM, alertM, emergencyM
   )
   where

--import System.IO ( stdout )
import System.Log.Formatter
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( fileHandler )
import System.Log.Logger


lname :: String
lname = "normal-output"


{- Set up logging
-}
initLogging :: FilePath -> Priority -> IO ()
initLogging logFile priority = do
   -- Remove the root logger's default handler that writes every
   -- message to stderr!
   updateGlobalLogger rootLoggerName removeHandler

   -- Set up our logger
   --h <- streamHandler stdout DEBUG
   h <- fileHandler logFile DEBUG >>= \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $prio] $msg")
   updateGlobalLogger lname $ setHandlers [h]
   updateGlobalLogger lname $ setLevel priority
