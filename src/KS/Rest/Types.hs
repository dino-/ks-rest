-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Rest.Types
   where

import           Data.Aeson ( FromJSON, ToJSON, Value )
import qualified Data.Text as T
import           GHC.Generics ( Generic )


newtype ByLocResults = ByLocResults [Value]
   deriving Generic

instance ToJSON ByLocResults


newtype PlaceIDs = PlaceIDs [T.Text]
   deriving (Generic, Show)

instance FromJSON PlaceIDs
instance ToJSON PlaceIDs
