{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Prelude hiding (sum)

connect_to_db :: IO Connection
connect_to_db =
  do
    connect
      defaultConnectInfo
        { connectHost = "database",
          connectDatabase = "motorola"
        }

query_db :: (ToRow q, FromRow r) => Query -> q -> Handler [r]
query_db q p =
  liftIO $
    do
      conn <- connect_to_db
      query conn q p

execute_db :: ToRow q => Query -> q -> Handler Int64
execute_db q p =
  liftIO $
    do
      conn <- connect_to_db
      execute conn q p

truncate_db :: IO ()
truncate_db =
  do
    conn <- connect_to_db
    execute_ conn "TRUNCATE DEVICE_PROFILES, LOCATIONS"
    return ()

type Location = String

type OptionalLocation = Maybe Location

type RadioID = Int

type RadioAlias = String

data DeviceProfileData = DPD
  { alias :: RadioAlias,
    allowed_locations :: [Location]
  }

data LocationData = LD
  { location :: Location
  }

$(deriveJSON defaultOptions ''DeviceProfileData)
$(deriveJSON defaultOptions ''LocationData)

type RadiosAPI =
  "radios" :> Capture "id" Int :> ReqBody '[JSON] DeviceProfileData :> Post '[JSON] ()
    :<|> "radios" :> Capture "id" Int :> "location" :> ReqBody '[JSON] LocationData :> Post '[JSON] ()
    :<|> "radios" :> Capture "id" Int :> "location" :> Get '[JSON] LocationData

startApp :: IO ()
startApp = run 8080 app


app :: Application
app = serve radioApi server

radioApi :: Proxy RadiosAPI
radioApi = Proxy

add_radio :: RadioID -> DeviceProfileData -> Handler ()
add_radio radioId dpd = do
  conn <- liftIO $ connect_to_db
  liftIO $
    withTransaction conn $
      do
        execute conn "INSERT INTO DEVICE_PROFILES (radio_id, alias) VALUES (?, ?)" (radioId, alias dpd)
        mapM_ (\loc -> execute conn "INSERT INTO LOCATIONS (device_profile_id, location_name) VALUES (?, ?)" (radioId, loc)) (allowed_locations dpd)
  return ()

set_radio_location :: RadioID -> LocationData -> Handler ()
set_radio_location radioId LD {location} =
  do
    conn <- liftIO $ connect_to_db
    loc <- liftIO $ query conn "SELECT location_id FROM LOCATIONS WHERE device_profile_id = ? and location_name = ?" (radioId, location) :: Handler [Only Int64]
    case loc of
      [Only loc_id] -> liftIO $ execute conn "UPDATE DEVICE_PROFILES SET location = ? WHERE radio_id = ?" (loc_id, radioId)
      _ -> throwError err403
    return ()

get_radio_location :: RadioID -> Handler LocationData
get_radio_location radioId = do
  location_list <- query_db "SELECT LOCATIONS.location_name FROM DEVICE_PROFILES INNER JOIN  LOCATIONS ON DEVICE_PROFILES.location=LOCATIONS.location_id  WHERE DEVICE_PROFILES.radio_id = ?" (Only radioId)
  case location_list of
    [Only location] ->
      return $ LD location
    _ -> throwError err404

server :: Server RadiosAPI
server =
  add_radio :<|> set_radio_location
    :<|> get_radio_location
