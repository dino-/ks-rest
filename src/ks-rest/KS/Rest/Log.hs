-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Log
   ( initLogging, lname
   , lineM

   -- Re-exported from System.Log
   , debugM, infoM, noticeM, warningM, errorM , criticalM, alertM, emergencyM

   -- Re-exported from Control.Monad.IO.Class
   , liftIO
   )
   where

import Control.Monad.IO.Class ( liftIO )
import Network.Wai ( Middleware )
import Network.Wai.Middleware.Gzip ( def )
import Network.Wai.Middleware.RequestLogger ( Destination (Logger)
   , OutputFormat (..), IPAddrSource (..), destination, mkRequestLogger
   , outputFormat )
import System.Log.FastLogger ( LoggerSet, defaultBufSize, newFileLoggerSet
   , pushLogStr, toLogStr )
import System.Log.Formatter ( nullFormatter, simpleLogFormatter )
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( GenericHandler (..) )
import System.Log.Logger


lname :: String
lname = rootLoggerName


initLogging :: Priority -> FilePath -> IO Middleware
initLogging prio logFilePath = do
   -- Remove the root logger's default handler that writes every
   -- message to stderr!
   updateGlobalLogger lname removeHandler
   updateGlobalLogger lname $ setLevel prio

   logDest <- newFileLoggerSet defaultBufSize logFilePath

   -- A WAI handler with timestamping
   (flip setFormatter $ simpleLogFormatter "[$time : $prio] $msg\n")
      <$> waiHandler logDest DEBUG
      >>= updateGlobalLogger lname . addHandler

   mkRequestLogger def
      { outputFormat = Apache FromFallback
      , destination = Logger logDest
      }


waiHandler :: LoggerSet -> Priority -> IO (GenericHandler LoggerSet)
waiHandler logDest prio = return $ GenericHandler
   { priority = prio
   , formatter = nullFormatter
   , privData = logDest
   , writeFunc = writeFuncImpl
   , closeFunc = \_ -> return ()
   }

   where
      writeFuncImpl logDest' msg = pushLogStr logDest' . toLogStr $ msg


lineM :: IO ()
lineM = noticeM lname $ replicate 60 '-'
