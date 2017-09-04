-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Rest.Handler.Feedback
   ( Feedback (..)
   , handler
   , missingFeedback
   , incorrectFeedback
   )
   where

import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Except ( ExceptT )
import Data.Bson.Generic ( toBSON )
import Data.Pool ( Pool, withResource )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB hiding ( options )
import KS.Data.Feedback ( Feedback (..)
   , IssueType (NoInspection, WrongNameAddr), Status (New) )
import Servant ( NoContent (..), ServantErr )
import Text.Printf ( printf )

import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Util ( coll_feedback, requiredParam, verifyAPIKey )


missingFeedback :: Feedback
missingFeedback = Feedback New "somedevice" (Just "ChIJS4cRvuHCrIkReU69l8QMHzM")
   (Just "Name Of Restaurant") 20160409 NoInspection Nothing


incorrectFeedback :: Feedback
incorrectFeedback = Feedback New "somedevice" Nothing Nothing 20160418
   WrongNameAddr (Just "123 Anystreet, Sometown USA")


handler :: Config -> Pool Pipe -> Maybe String
   -> Feedback
   -> ExceptT ServantErr IO NoContent
handler conf pool mbKey feedback = do
   liftIO $ lineM

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead

   liftIO $ infoM lname $ printf
      "Feedback received: %s" (show feedback)

   result <- withResource pool (\pipe ->
      access pipe slaveOk (database . mongoConf $ conf) $
         save coll_feedback (toBSON feedback) >> lastStatus
      )

   liftIO $ infoM lname $ printf "Feedback inserted, result: %s" $ either id id result

   return NoContent
