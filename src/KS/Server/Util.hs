-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Server.Util
   ( requiredParam
   )
   where

import Control.Monad.Trans.Either ( EitherT, left )
import Data.ByteString.Lazy ( ByteString, concat )
import Prelude hiding ( concat )
import Servant ( ServantErr (errBody), err400 )


{- Attempt to extract the a from a parameter (Maybe a) value and
   throw an error in the Servant ErrorT monad if it's Nothing
-}
requiredParam :: ByteString -> Maybe a -> EitherT ServantErr IO a
requiredParam paramName = maybe (left $ err400 {
   errBody = concat ["Missing required query param: ", paramName] })
   return
