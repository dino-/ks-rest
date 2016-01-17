-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import           Control.Monad ( when )
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Digest.Pure.SHA ( sha1, showDigest )
import           Data.List ( intercalate )
import           Prelude hiding ( words )
import           System.Environment ( getArgs )

import           KS.Server.APIKey


main :: IO ()
main = do
   allArgs <- getArgs

   -- There has to be some input to hash
   when (length allArgs < 4) $ error $
      "ERROR: invalid input\n"
      ++ "usage: ks-api-keygen (ro|rw) NAME DESC WORD [WORD WORD...]"

   let (typeStr : name : desc : words) = allArgs
   let keyType = getKeyType typeStr

   -- Jam the words together into one, space-delimited String
   let inputStr = intercalate " " words

   -- Hash it with SHA1 and print the hex form
   let keyValue = showDigest . sha1 . BL.pack $ inputStr

   print $ APIKey keyValue name desc keyType
