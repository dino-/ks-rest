-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.APIKey
   ( APIKey (..)
   , getKeyType
   , verifyReadOnly
   , verifyReadWrite
   )
   where

import Text.Printf ( printf )


type APIKeyValue = String
type APIKeyName = String
type APIKeyDesc = String

data APIKeyType = ReadOnly | ReadWrite | Revoked
   deriving (Read, Show)

data APIKey = APIKey
   { akKey :: APIKeyValue
   , akName :: APIKeyName
   , akDesc :: APIKeyDesc
   , akType :: APIKeyType
   }
   deriving (Read, Show)

type APIKeys = [APIKey]


findAPIKey :: APIKeyValue -> APIKeys -> Either String APIKey
findAPIKey keyValue [] = Left $ "Unable to find key: " ++ keyValue
findAPIKey keyValue (k:ks)
   | keyValue == akKey k = Right k
   | otherwise = findAPIKey keyValue ks


isReadOnly :: APIKey -> Either String ()
isReadOnly (APIKey _        _ _ ReadOnly) = Right ()
isReadOnly (APIKey keyValue _ _ ktype   ) =
   Left $ printf "Key %s is not ReadOnly" keyValue


isReadWrite :: APIKey -> Either String ()
isReadWrite (APIKey _        _ _ ReadWrite) = Right ()
isReadWrite (APIKey keyValue _ _ ktype    ) =
   Left $ printf "Key %s is not ReadWrite" keyValue


verifyReadOnly :: APIKeyValue -> APIKeys -> Either String ()
verifyReadOnly keyValue keys = findAPIKey keyValue keys >>= isReadOnly


verifyReadWrite :: APIKeyValue -> APIKeys -> Either String ()
verifyReadWrite keyValue keys = findAPIKey keyValue keys >>= isReadWrite


getKeyType :: String -> APIKeyType
getKeyType "ro" = ReadOnly
getKeyType "rw" = ReadWrite
getKeyType _    = undefined
