-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances,
    MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Aeson ( Value, toJSON, (.=), decode, object )
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import           Data.Version ( showVersion )
import           Database.MongoDB ( Limit, Pipe
                  , access, auth, connect, host, slaveOk )
import           Network.Wai.Handler.Warp ( run )
import           Paths_ks_rest ( version )
import           Servant
import           Servant.Docs
import           System.Environment ( getArgs )
import           System.Exit ( exitSuccess )
import           Text.Printf ( printf )

import           KS.Rest.Config
                  ( Config ( logPath, logPriority, mongoConf, webServerPort )
                  , MongoConf ( database, ip, password, username )
                  , loadConfig
                  )
import qualified KS.Data.Document as D
import qualified KS.Rest.Handler.InspAllName
import qualified KS.Rest.Handler.InspAllPlaceIDCap
import qualified KS.Rest.Handler.InspRecentNear
import qualified KS.Rest.Handler.InspRecentPlaceID
import qualified KS.Rest.Handler.InspSorted
import qualified KS.Rest.Handler.StatsLatest
import qualified KS.Rest.Handler.Version
import           KS.Rest.Types ( ByLocResults (..), PlaceIDs (..) )
import           KS.Rest.Util ( coll_inspections_all, coll_inspections_recent )
import           KS.Rest.Log ( initLogging, lineM, lname, noticeM )


type APIVer = "v1.1"

type KSAPI
   -- search, by loc
   =     APIVer :> "inspections" :> "recent" :> "near" :>
         QueryParam "key"        String :>
         QueryParam "lat"        Double :>
         QueryParam "lng"        Double :>
         QueryParam "dist"       Double :>
         QueryParam "min_score"  Double :>
         Get '[JSON] ByLocResults

   -- search, place name matches regex
   :<|>  APIVer :> "inspections" :> "all" :> "name" :>
         QueryParam "key"     String :>
         QueryParam "regex"   T.Text :>
         Get '[JSON] [D.Document]

   -- history of one particular place
   :<|>  APIVer :> "inspections" :> "all" :> "placeid" :> Capture "placeid" T.Text :>
         QueryParam "key" String :>
         Get '[JSON] [D.Document]

   -- when sorted by score, used for high/low
   :<|>  APIVer :> "inspections" :> "recent" :> "sorted" :>
         QueryParam "key"     String :>
         QueryParam "lat"     Double :>
         QueryParam "lng"     Double :>
         QueryParam "dist"    Double :>
         QueryParam "limit"   Limit :>
         QueryParam "sort"    T.Text :>
         Get '[JSON] [D.Document]

   -- when sorted by date, used for latest
   :<|>  APIVer :> "inspections" :> "all" :> "sorted" :>
         QueryParam "key"     String :>
         QueryParam "lat"     Double :>
         QueryParam "lng"     Double :>
         QueryParam "dist"    Double :>
         QueryParam "limit"   Limit :>
         QueryParam "sort"    T.Text :>
         Get '[JSON] [D.Document]

   -- favorites
   :<|>  APIVer :> "inspections" :> "recent" :> "placeid" :> Capture "placeid" T.Text :>
         QueryParam "key"     String :>
         QueryParam "after"   Int :>
         Get '[JSON] [D.Document]

   -- favorites
   :<|>  APIVer :> "inspections" :> "recent" :> "placeid" :>
         QueryParam "key"     String :>
         QueryParam "after"   Int :>
         ReqBody '[JSON] PlaceIDs :>
         Post '[JSON] [D.Document]

   -- old stats call
   :<|>  APIVer :> "stats" :> "latest" :> "by_source" :>
         QueryParam "key"     String :>
         QueryParam "sources" T.Text :>
         Get '[JSON] [Value]

   -- new stats call
   :<|>  APIVer :> "stats" :> "recent" :> "near" :>
         QueryParam "key"     String :>
         QueryParam "lat"     Double :>
         QueryParam "lng"     Double :>
         QueryParam "dist"    Double :>
         Get '[JSON] [Value]

   :<|>  APIVer :> "version" :>
         Get '[JSON] Value


server :: Config -> Pipe -> Server KSAPI
server conf pipe
   =     KS.Rest.Handler.InspRecentNear.handler conf pipe
   :<|>  KS.Rest.Handler.InspAllName.handler conf pipe
   :<|>  KS.Rest.Handler.InspAllPlaceIDCap.handler conf pipe
   :<|>  KS.Rest.Handler.InspSorted.handler conf pipe coll_inspections_recent
   :<|>  KS.Rest.Handler.InspSorted.handler conf pipe coll_inspections_all
   :<|>  KS.Rest.Handler.InspRecentPlaceID.handlerCapture conf pipe
   :<|>  KS.Rest.Handler.InspRecentPlaceID.handlerPost conf pipe
   :<|>  KS.Rest.Handler.StatsLatest.handlerBySource conf pipe
   :<|>  KS.Rest.Handler.StatsLatest.handlerRecentNear conf pipe
   :<|>  KS.Rest.Handler.Version.handler


ksAPI :: Proxy KSAPI
ksAPI = Proxy


main :: IO ()
main = do
   confDir <- parseArgs

   config <- loadConfig confDir

   {- initLogging both sets up the hs-logger logging instance and
      also returns a WAI Middleware for use below

      MiddleWare :: Application -> Application
   -}
   logger <- initLogging (logPriority config) (logPath config)

   let port = webServerPort config

   lineM
   noticeM lname $ printf "ks-server version %s started on port %d"
      (showVersion version) port

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   -- 'serve' comes from servant and hands you a WAI Application,
   -- which you can think of as an 'abstract' web application,
   -- not yet a webserver.
   -- app :: Application
   let app = logger $ serve ksAPI (server config pipe)

   run port app

   {- These never execute, is that bad? Consider catching the ctrl-c..

   putStrLn "Server shutting down..."
   close pipe
   -}


parseArgs :: IO String
parseArgs = do
   (arg : _) <- getArgs

   if (arg == "--generate-api-docs")
      -- User asked for API docs, print them and get out of here
      then do
         putStrLn . markdown $ apiDocs
         exitSuccess
      -- Normal operation, return the config dir path we got
      else return arg


-- Documentation

instance ToParam (QueryParam "key" String) where
   toParam _ = DocQueryParam
      "key"                                        -- name
      ["c6d4376da7119afff1de3d5af43723b8afcc3a85"] -- example values
      "API key"
      Normal

instance ToParam (QueryParam "lat" Double) where
   toParam _ = DocQueryParam
      "lat"                              -- name
      ["35.7819225"]                     -- example values
      ("Latitude value")
      Normal

instance ToParam (QueryParam "lng" Double) where
   toParam _ = DocQueryParam
      "lng"                              -- name
      ["-78.6484261"]                    -- example values
      ("Longitude value")
      Normal

instance ToParam (QueryParam "dist" Double) where
   toParam _ = DocQueryParam
      "dist"                              -- name
      ["2000", "80000"]                   -- example values
      ("Distance in meters for location-based searches")
      Normal

instance ToParam (QueryParam "min_score" Double) where
   toParam _ = DocQueryParam
      "min_score"                         -- name
      ["97.5"]                            -- example values
      ("The minimum inspection score cut-off, only values higher than this will be returned. Defaults to " ++ (show KS.Rest.Handler.InspRecentNear.defaultMinScore))
      Normal

instance ToParam (QueryParam "sort" T.Text) where
   toParam _ = DocQueryParam
      "sort"                        -- name
      ["+score", "-score", "-date"] -- example values
      "Sort direction and inspection field"
      Normal

instance ToParam (QueryParam "regex" T.Text) where
   toParam _ = DocQueryParam
      "regex"                             -- name
      ["bloomsbury", "taco"]              -- example values
      "Inspections whose place name matches a regular expression"
      Normal

instance ToCapture (Capture "placeid" T.Text) where
   toCapture _ = DocCapture
      "placeid"                                                   -- name
      "Google Places ID, example: `ChIJT6iXT3JfrIkRIga2syiYuGM`"  -- description


instance ToParam (QueryParam "after" Int) where
   toParam _ = DocQueryParam
      "after"                                               -- name
      ["1453166568", "230143443"]                           -- example values
      "A date before which we can filter out inspections. Defaults to 0 (January 1st, 1970)"
      Normal

instance ToSample PlaceIDs PlaceIDs where
   toSample _ = Just $ PlaceIDs
      [ "ChIJT6iXT3JfrIkRIga2syiYuGM"
      , "ChIJwe_MrOP3rIkRkzWAFA26Kt8"
      ]

-- FIXME This goes away when we redesign stats
instance ToParam (QueryParam "sources" T.Text) where
   toParam _ = DocQueryParam
      "sources"                           -- name
      ["nc_wake", "nc_wake,nc_orange"]    -- example values
      "Inspection source regions to limit search to"
      Normal

instance ToParam (QueryParam "limit" Limit) where
   toParam _ = DocQueryParam
      "limit"                             -- name
      ["50", "200"]                       -- example values
      ("Number of inspections to limit response to. Defaults to " ++ (show KS.Rest.Handler.InspSorted.defaultLimit))
      Normal

instance ToSample ByLocResults ByLocResults where
   toSample _ = Just $ ByLocResults
      [ object
         [ "obj" .= (toJSON inspMonkeyJoes)
         , "dis" .= (628.5469 :: Double)
         ]
      ]

instance ToSample [D.Document] [D.Document] where
   toSample _ = Just $ [inspMonkeyJoes]

instance ToSample [Value] [Value] where
   toSample _ = Just $
      [ object
         [ "_id" .= ("567589fac0b05b84bdea0e45" :: T.Text)
         , "doctype" .= ("regional_stats" :: T.Text)
         , "source" .= ("nc_wake" :: T.Text)
         , "state" .= ("NC" :: T.Text)
         , "county" .= ("Wake" :: T.Text)
         , "date" .= (1452321061 :: Int)
         , "count_total" .= (2013 :: Int)
         , "count_a1" .= (198 :: Int)
         , "count_a2" .= (424 :: Int)
         , "count_a3" .= (795 :: Int)
         , "count_a4" .= (589 :: Int)
         , "count_b" .= (7 :: Int)
         , "count_c" .= (0 :: Int)
         , "min_score" .= (84 :: Int)
         , "max_score" .= (100 :: Int)
         , "avg_score" .= (95.68157196044922 :: Double)
         ]
      ]

instance ToSample Value Value where
   toSample _ = Just $ object
      [ "ks_rest_api_version" .= ("1.0" :: T.Text)
      , "ks_rest_server_version" .= ("1.10" :: T.Text)
      ]


apiDocs :: API
apiDocs = docsWithIntros [intro] ksAPI where
   intro = DocIntro "REST API documentation" []


inspMonkeyJoes :: D.Document
inspMonkeyJoes = fromJust . decode $ "{\"inspection\":{\"violations\":4,\"score\":94,\"addr\":\"6220 GLENWOOD AVE RALEIGH, NC 27612\",\"date\":1452142800,\"inspection_source\":\"nc_wake\",\"name\":\"MONKEY JOE`S #168\",\"crit_violations\":2,\"detail\":\"http://wake-nc.healthinspections.us/_templates/NC/Restaurant/_report_full.cfm?reportID=2AFAC28D-5056-A20B-FAC7E1EB90DCB6FA&facilityID=1F0349EE-3048-98E1-5C78ECD1E3B5BC97&rtype=Restaurant&domainID=15&ps=ps\",\"reinspection\":false},\"doctype\":\"inspection\",\"place\":{\"vicinity\":\"6220 Glenwood Avenue #104, Raleigh\",\"location\":{\"coordinates\":[-78.70109699999999,35.858112],\"type\":\"Point\"},\"types\":[\"point_of_interest\",\"establishment\"],\"place_id\":\"ChIJueTlwFX2rIkRhDVrnDQ3BJY\",\"name\":\"Monkey Joe's\"}}"
