{-# LANGUAGE OverloadedStrings #-}

module Controller (
        mainRouter -- we only need to export mainRouter for Main.hs
        ) where

import Database
import Model
import View
import Snap
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Class


mainRouter = route [
  ("", method GET businessRouteIndex) -- gets a list of businesses
  -- Add more routes here and write functions below to handle the logic
                   ]

businessRouteIndex = do
  -- Get limit and start of query from (?limit=:limit&start=:start) if sent
  maybeLimitTo <- getParam "limit"
  maybeOffsetBy <- getParam "start"

  -- Get a list or array of business from the database
  businesses <- liftIO $ getBusinesses maybeLimitTo maybeOffsetBy

  modifyResponse $ setHeader "Content-Type" "application/json"
  -- Write out JSON rsponse
  writeLBS $ encode Prelude.map entityIdToJSON businesses
