-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.Environment ( getArgs )

import qualified KS.Server.Config as C


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- C.loadConfig confDir
   print config
