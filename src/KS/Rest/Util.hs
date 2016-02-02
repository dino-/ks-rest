-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Util
   ( metersToEarthRadians
   , parseSortParam
   , requiredParam
   , verifyAPIKey
   )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT, left )
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.ByteString.Lazy ( ByteString, concat )
import qualified Data.Text as T
import           Database.MongoDB ( Order, (=:) )
import           Prelude hiding ( concat )
import           Servant
                  ( ServantErr (errBody)
                  , err400    -- Bad Request
                  , err401    -- Unauthorized
                  )

import qualified KS.Rest.APIKey as AK
import           KS.Rest.Config ( Config (apiKeys) )
import           KS.Rest.Log ( infoM, lname, noticeM )


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
         left $ err401 { errBody = C.pack msg}
      Right _  -> do
         liftIO $ infoM lname
            $ "Successful authentication with API key " ++ keyValue
         return keyValue


metersToEarthRadians :: Double -> Double
metersToEarthRadians m = (mToKm m) / earthRadiansPerKm
   where
      -- Convert meters to kilometers
      mToKm m' = m' * 0.001

      -- Number of radians on the Earth coordinate system in
      -- a kilometer
      earthRadiansPerKm = 6378.1

      -- Number of radians on the Earth coordinate system in
      -- a mile. Not used, but in case we have a future need.
      --earthRadiansPerMile = 3963.2


parseSortParam :: T.Text -> EitherT ServantErr IO Order
parseSortParam paramValue = do
   ordering <- convertOrdering $ T.take 1 paramValue
   field <- verifyField $ T.drop 1 paramValue
   return $ [ (T.concat ["inspection.", field]) =: ordering ]

   where
      convertOrdering :: T.Text -> EitherT ServantErr IO Int
      convertOrdering "+" = return 1
      convertOrdering "-" = return (-1)
      convertOrdering o   = left $ err400 { errBody = C.concat
         [ "Cannot parse sort parameter because we got sort ordering '"
         , (C.pack . T.unpack $ o)
         , "' but expected '+' or '-'"
         ] }

      validSortFields :: [T.Text]
      validSortFields = ["date", "score"]

      verifyField :: T.Text -> EitherT ServantErr IO T.Text
      verifyField field
         | field `elem` validSortFields = return field
         | otherwise = left $ err400 { errBody = C.concat
            [ "Cannot parse sort parameter because we got sort field '"
            , (C.pack . T.unpack $ field)
            , "' but expected one of "
            , (C.pack . show $ validSortFields)
            ] }
