-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Environment ( getArgs )

import KS.Server.Config
import KS.Server.Log


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   initLogging (logPriority config) (logPath config)

   noticeM lname "ks-server started"
