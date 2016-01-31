-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Rest.Types
   where

import Data.Aeson ( ToJSON, Value )
import GHC.Generics ( Generic )


newtype ByLocResults = ByLocResults [Value]
   deriving Generic

instance ToJSON ByLocResults
