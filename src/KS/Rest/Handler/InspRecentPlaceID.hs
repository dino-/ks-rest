-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Rest.Handler.InspRecentPlaceID
   ( defaultDateAfter
   , handlerCapture, handlerPost
   )
   where

import           Control.Monad.Trans ( liftIO )
import           Control.Monad.Trans.Either ( EitherT )
import           Data.Bson.Generic ( fromBSON )
import           Data.Maybe ( catMaybes )
import qualified Data.Text as T
import           Database.MongoDB hiding ( options )
import           Servant ( ServantErr )
import           Text.Printf ( printf )

import qualified KS.Data.Document as D
import           KS.Rest.APIKey ( akRead )
import           KS.Rest.Config ( Config (mongoConf), MongoConf (database) )
import           KS.Rest.Log ( infoM, lineM, lname )
import           KS.Rest.Types ( PlaceIDs (..) )
import           KS.Rest.Util ( coll_inspections_recent, requiredParam, verifyAPIKey )


defaultDateAfter :: Int
defaultDateAfter = 19700101


handlerCapture :: Config -> Pipe -> T.Text -> Maybe String -> Maybe Int
   -> EitherT ServantErr IO [D.Document]
handlerCapture conf pipe placeID mbKey mbAfter =
   handlerPost conf pipe mbKey mbAfter (PlaceIDs [placeID])


handlerPost :: Config -> Pipe -> Maybe String -> Maybe Int
   -> PlaceIDs
   -> EitherT ServantErr IO [D.Document]
handlerPost conf pipe mbKey mbAfter (PlaceIDs placeIDs) = do
   liftIO $ lineM

   _ <- requiredParam "key" mbKey >>= verifyAPIKey conf akRead
   let dateAfter = maybe defaultDateAfter id mbAfter

   liftIO $ infoM lname $ printf
      "by_placeid received, dateAfter %d, placeids: %s"
      dateAfter (show placeIDs)

   ds <- access pipe slaveOk (database . mongoConf $ conf) $ rest =<<
      find (select
         [ "place.place_id" =: [ "$in" =: placeIDs ]
         , "inspection.date" =: [ "$gte" =: dateAfter ]
         ] coll_inspections_recent)
         { sort = [ "inspection.date" =: (-1 :: Int) ] }

   liftIO $ infoM lname $ printf "Retrieved %d inspections" $ length ds

   return $ catMaybes . map fromBSON $ ds
