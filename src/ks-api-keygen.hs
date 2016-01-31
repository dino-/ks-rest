-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import           Control.Monad ( when )
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Digest.Pure.SHA ( sha1, showDigest )
import           Data.List ( intercalate )
import           Data.Version ( showVersion )
import           Paths_ks_rest ( version )
import           Prelude hiding ( words )
import           System.Environment ( getArgs )

import           KS.Rest.APIKey ( APIKey (..) )


main :: IO ()
main = do
   allArgs <- getArgs

   -- There has to be some input to hash
   when (length allArgs < 4) $ error $ unlines $
      [ "ERROR: invalid input"
      , "usage: ks-api-keygen PERMS NAME DESC WORD [WORD WORD...]"
      , ""
      , "PERMS is a number representing permissions bits on or off"
      , "   akRead    = 2"
      , "   akWrite   = 1"
      , "   akRevoked = 0"
      , ""
      , "So, read-only is 2, read-write is 3 and revoked is 0"
      , ""
      , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
      ]

   let (permsStr : name : desc : words) = allArgs
   let perms = read permsStr

   -- Jam the words together into one, space-delimited String
   let inputStr = intercalate " " words

   -- Hash it with SHA1 and print the hex form
   let keyValue = showDigest . sha1 . BL.pack $ inputStr

   print $ APIKey keyValue name desc perms
