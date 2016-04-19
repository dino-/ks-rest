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
import Data.Aeson ( FromJSON, ToJSON )
import Data.Bson.Generic ( FromBSON ,ToBSON, fromBSON, toBSON )
import Data.Pool ( Pool, withResource )
import qualified Data.Text as T
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB hiding ( options )
import GHC.Generics ( Generic )
import Servant ( ServantErr )
import Text.Printf ( printf )

import KS.Rest.APIKey ( akRead )
import KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import KS.Rest.Log ( infoM, lineM, lname )
import KS.Rest.Util ( coll_feedback, requiredParam, verifyAPIKey )


data Status = New | Duplicate | Resolved
   deriving (Eq, Generic, Read, Show)

instance FromJSON Status
instance ToJSON Status

instance FromBSON Status
instance ToBSON Status


data IssueType
   = Closed | WrongNameAddr | InspectionNotCurrent | Other | NoInspection
   deriving (Eq, Generic, Read, Show)

instance FromJSON IssueType
instance ToJSON IssueType

instance FromBSON IssueType
instance ToBSON IssueType


data Feedback = Feedback
   { status :: Status
   , device_id :: T.Text
   , place_id :: Maybe T.Text
   , name :: Maybe T.Text
   , date :: Int
   , issue_type :: IssueType
   , comment :: Maybe T.Text
   }
   deriving (Eq, Generic, Show)

instance FromJSON Feedback
instance ToJSON Feedback

instance FromBSON Feedback where
   fromBSON doc = Just $ Feedback
      (read ("status" `at` doc))
      ("device_id" `at` doc)
      ("place_id" `at` doc)
      ("name" `at` doc)
      (read ("date" `at` doc))
      (read ("issue_type" `at` doc))
      ("comment" `at` doc)

instance ToBSON Feedback where
   toBSON adt =
      [ "status" =: (show (status adt))
      , "device_id" =: (device_id adt)
      , "place_id" =: (place_id adt)
      , "name" =: (name adt)
      , "date" =: (show (date adt))
      , "issue_type" =: (show (issue_type adt))
      , "comment" =: (comment adt)
      ]


missingFeedback :: Feedback
missingFeedback = Feedback New "somedevice" (Just "ChIJS4cRvuHCrIkReU69l8QMHzM")
   (Just "Name Of Restaurant") 20160409 NoInspection Nothing


incorrectFeedback :: Feedback
incorrectFeedback = Feedback New "somedevice" Nothing Nothing 20160418
   WrongNameAddr (Just "123 Anystreet, Sometown USA")


handler :: Config -> Pool Pipe -> Maybe String
   -> Feedback
   -> ExceptT ServantErr IO ()
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
