-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.APIKey
   ( APIKeys, APIKey (..), APIKeyValue, APIKeyPermissions
   , akRead, akWrite, akRevoked
   , isSet
   , verifyAPIKey
   )
   where

import Data.Bits ( (.&.) )
import Text.Printf ( printf )


type APIKeyValue = String
type APIKeyName = String
type APIKeyDesc = String

type APIKeyPermissions = Int

data APIKey = APIKey
   { akKey :: APIKeyValue
   , akName :: APIKeyName
   , akDesc :: APIKeyDesc
   , akPerms :: APIKeyPermissions
   }
   deriving (Read, Show)

type APIKeys = [APIKey]


akRead, akWrite, akRevoked :: APIKeyPermissions
akRead      = 0x02
akWrite     = 0x01
akRevoked   = 0x00


findAPIKey :: APIKeys -> APIKeyValue -> Either String APIKey
findAPIKey []     keyValue = Left $ "Unable to find API key: " ++ keyValue
findAPIKey (k:ks) keyValue
   | keyValue == akKey k = Right k
   | otherwise = findAPIKey ks keyValue


isSet :: APIKeyPermissions -> APIKeyPermissions -> Bool
isSet desiredPerms existingPerms =
   desiredPerms .&. existingPerms == desiredPerms


verifyPermissions :: APIKeyPermissions -> APIKey -> Either String APIKey
verifyPermissions desiredPerms ak@(APIKey keyValue _ _ actualPerms)
   | isSet desiredPerms actualPerms = Right ak
   | otherwise = Left $ printf
      "Cannot verify API key %s because its permissions are %d but we are expecting %d"
      keyValue actualPerms desiredPerms


verifyAPIKey :: APIKeyPermissions -> APIKeys -> APIKeyValue -> Either String APIKey
verifyAPIKey desiredPerms keys keyValue =
   findAPIKey keys keyValue >>= verifyPermissions desiredPerms
