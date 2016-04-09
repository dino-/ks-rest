-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Util
   ( coll_feedback
   , coll_inspections_all, coll_inspections_recent
   , coll_stats_recent
   , requiredParam
   , verifyAPIKey
   )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Except ( ExceptT, throwE )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.ByteString.Lazy ( ByteString, concat )
import Database.MongoDB ( Collection )
import Prelude hiding ( concat )
import Servant
   ( ServantErr (errBody)
   , err400    -- Bad Request
   , err401    -- Unauthorized
   )

import qualified KS.Rest.APIKey as AK
import KS.Rest.Config ( Config (apiKeys) )
import KS.Rest.Log ( infoM, lname, noticeM )


coll_feedback, coll_inspections_all, coll_inspections_recent, coll_stats_recent :: Collection
coll_feedback           = "feedback"
coll_inspections_all    = "inspections_all"
coll_inspections_recent = "inspections_recent"
coll_stats_recent       = "stats_recent"


{- Attempt to extract the a from a parameter (Maybe a) value and
   throw an error in the Servant ErrorT monad if it's Nothing
-}
requiredParam :: ByteString -> Maybe a -> ExceptT ServantErr IO a
requiredParam paramName = maybe (throwE $ err400 {
   errBody = concat ["Missing required query param: ", paramName] })
   return


{- Verify that an API key has the proper type
-}
verifyAPIKey :: Config -> AK.APIKeyPermissions -> AK.APIKeyValue
   -> ExceptT ServantErr IO AK.APIKey
verifyAPIKey conf perms keyValue =
   case (AK.verifyAPIKey perms (apiKeys conf) keyValue) of
      Left msg -> do
         liftIO $ noticeM lname msg
         throwE $ err401 { errBody = pack msg}
      Right ak  -> do
         liftIO $ noticeM lname
            $ "Successful authentication with API key: " ++ keyValue
         liftIO $ infoM lname
            $ "Full API key details: " ++ (show ak)
         return ak
