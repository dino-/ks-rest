-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( when )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Data.List ( intercalate )
import System.Environment ( getArgs )


main :: IO ()
main = do
   -- Jam the args together into one, space-delimited String
   inputStr <- intercalate " " <$> getArgs

   -- There has to be some input to hash
   when (null inputStr) $ error $
      "ERROR: Can't generate a hash because no input was given\n"
      ++ "   usage: ks-api-keygen WORD [WORD WORD...]"

   -- Hash it with SHA1 and print the hex form
   putStrLn . showDigest . sha1 . BL.pack $ inputStr
