-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DataKinds, DeriveGeneric, TypeFamilies, TypeOperators #-}

import           Data.Aeson ( Value )
import qualified Data.Text as T
import           Data.Version ( showVersion )
import           Database.MongoDB ( Limit, Pipe
                  , access, auth, connect, host, slaveOk )
import           Network.Wai.Handler.Warp ( run )
import           Paths_ks_server ( version )
import           Servant
import           System.Environment ( getArgs )

import           KS.Server.Config
                  ( Config ( logPath, logPriority, mongoConf)
                  , MongoConf ( database, ip, password, username )
                  , loadConfig
                  )
import qualified KS.Data.Document as D
import qualified KS.Server.Inspections.ByLoc
import qualified KS.Server.Inspections.ByName
import qualified KS.Server.Inspections.ByPlaceID
import qualified KS.Server.Inspections.BySource
import qualified KS.Server.Stats.Latest
import qualified KS.Server.Version
import           KS.Server.Log ( initLogging, lineM, lname, noticeM )


type KSAPI
   =     "v1.0" :> "inspections" :> "by_loc" :>
         QueryParam "pt"         T.Text :>
         QueryParam "dist"       Double :>
         QueryParam "min_score"  Double :>
         Get '[JSON] [Value]

   :<|>  "v1.0" :> "inspections" :> "by_name" :>
         QueryParam "regex" T.Text :>
         Get '[JSON] [D.Document]

   :<|>  "v1.0" :> "inspections" :> "by_placeid" :> Capture "placeid" T.Text :>
         Get '[JSON] [D.Document]

   :<|>  "v1.0" :> "inspections" :> "by_source" :> Capture "criteria" T.Text :>
         QueryParam "sources" T.Text :>
         QueryParam "limit"   Limit :>
         Get '[JSON] [D.Document]

   :<|>  "v1.0" :> "stats" :> "latest" :> "by_source" :>
         QueryParam "sources" T.Text :>
         Get '[JSON] [Value]

   :<|>  "v1.0" :> "version" :>
         Get '[JSON] Value


server :: MongoConf -> Pipe -> Server KSAPI
server mc pipe
   =     KS.Server.Inspections.ByLoc.handler mc pipe
   :<|>  KS.Server.Inspections.ByName.handler mc pipe
   :<|>  KS.Server.Inspections.ByPlaceID.handler mc pipe
   :<|>  KS.Server.Inspections.BySource.handler mc pipe
   :<|>  KS.Server.Stats.Latest.handler mc pipe
   :<|>  KS.Server.Version.handler


ksAPI :: Proxy KSAPI
ksAPI = Proxy


main :: IO ()
main = do
   (confDir : _) <- getArgs

   config <- loadConfig confDir

   {- initLogging both sets up the hs-logger logging instance and
      also returns a WAI Middleware for use below

      MiddleWare :: Application -> Application
   -}
   logger <- initLogging (logPriority config) (logPath config)

   lineM
   noticeM lname $ "ks-server version " ++ (showVersion version) ++ " started"

   let mc = mongoConf config

   pipe <- connect . host . ip $ mc

   (access pipe slaveOk (database mc)
      $ auth (username mc) (password mc)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   -- 'serve' comes from servant and hands you a WAI Application,
   -- which you can think of as an 'abstract' web application,
   -- not yet a webserver.
   -- app :: Application
   let app = logger $ serve ksAPI (server mc pipe)

   run 8081 app

   {- These never execute, is that bad? Consider catching the ctrl-c..

   putStrLn "Server shutting down..."
   close pipe
   -}
