-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Environment ( getArgs )

import KS.Server.Config
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   initLogging (logPath config) (logPriority config)

   noticeM lname "ks-server started"
   debugM lname "This is a debug message"
