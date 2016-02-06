-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Util
   ( coll_inspections_all, coll_inspections_recent
   , requiredParam
   , verifyAPIKey
   )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import           Data.ByteString.Lazy.Char8 ( pack )
import           Data.ByteString.Lazy ( ByteString, concat )
import           Database.MongoDB ( Collection )
import           Prelude hiding ( concat )
import           Servant
                  ( ServantErr (errBody)
                  , err400    -- Bad Request
                  , err401    -- Unauthorized
                  )

import qualified KS.Rest.APIKey as AK
import           KS.Rest.Config ( Config (apiKeys) )
import           KS.Rest.Log ( infoM, lname, noticeM )


coll_inspections_all, coll_inspections_recent :: Collection
coll_inspections_all = "inspections"
coll_inspections_recent = "recent_inspections"


{- Attempt to extract the a from a parameter (Maybe a) value and
   throw an error in the Servant ErrorT monad if it's Nothing
-}
requiredParam :: ByteString -> Maybe a -> EitherT ServantErr IO a
requiredParam paramName = maybe (left $ err400 {
   errBody = concat ["Missing required query param: ", paramName] })
   return


{- Very that an API key has the proper type
-}
verifyAPIKey :: Config -> AK.APIKeyPermissions -> AK.APIKeyValue
   -> EitherT ServantErr IO AK.APIKeyValue
verifyAPIKey conf perms keyValue =
   case (AK.verifyAPIKey perms (apiKeys conf) keyValue) of
      Left msg -> do
         liftIO $ noticeM lname msg
         left $ err401 { errBody = pack msg}
      Right _  -> do
         liftIO $ infoM lname
            $ "Successful authentication with API key " ++ keyValue
         return keyValue
