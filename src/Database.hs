 {-# LANGUAGE OverloadedStrings #-} -- Language Extension

-- export these modules for use in Main.hs and Controller.hs

module Database (
     dbMigration
     ) where

-- import our custom modules: Model and View

import Model
import View


-- import our build dependencies

import System.Environment -- to get DB connection string

-- Imports to handle strings and ints

import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy

-- Import for fromMaybe

import Data.Maybe

-- Import for JSON

import Data.Aeson

-- used for withDbRun type signature

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Logger

-- Needed to interface with

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sqlite as DbSql



-- Gather the database connection string from the environment
-- if non is found use the default

sqliteConnString = do
  maybeDbConnString <- lookupEnv "IZZI_BACKEND_DB_CONN"
  return $ Data.Text.pack $ fromMaybe "izzi_default.db" maybeDbConnString


-- Needed for each DB transaction (inserting, updating, retrieval, deleting)

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun command = do
  connString <- sqliteConnString
  runSqlite connString command

-- This will create the Business table
-- Persisten will help update the table schema if the model changes

dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entifyDef (Nothing :: Maybe Business)

getBusinessIdKey :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Business]
getBusinessIdKey maybeIdBS = toSqlKey businessIdInt64
  where
    -- if we get back Nothing for the id, pass back invalid id -1
    businessIdBS = fromMaybe ("-1" :: Data.ByteString.ByteString) maybeIdBS

    -- Convert the string to the needed 64 bit integer
    businessIdInt64 = read (Data.ByteString.Char8.unpack businessIdBS) :: Int64


-- Retrieves multiple business from start to the limit
getBusinesses maybeLimitTo maybeOffsetBy = do
  -- if the limit and the offset are 'Nothing' use defaults '10' and '0'
  let limitToBS = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy

  -- Converts strings to ints
  let limitToInt = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int

  -- actual DB call
  withDbRun $ DbSql.selectList ([] :: [Filter Business]) [LimitTo limitToInt, OffsetBy offsetByInt]


getBusinessById :: Maybe Data.ByteString.ByteString -> IO (Key Business, Maybe Business)
getBusinessById maybeIdBS = do
  -- get business primary key
  let businessIdKey = getBusinessIdKey maybeIdBS
  -- Get business from DB
  maybeBusiness <- withDbRun $ DbSql.get businessIdKey
  -- return the primary key and the business (if business exists)
  return (businessIdKey, maybeBusiness)


-- Create new business row in the DB
insertBusiness :: Business -> IO (Key Business)
insertBusiness business = withDbRun $ DbSql.insert business


-- Update the business info and store the updated copy
updateBusinessById maybeIdBS businessJSON = do
  let businessIdKey = getBusinessIdKey maybeIdBS
  (businessKeyId, maybeBusiness) <- getBusinessById maybeIdBS
  case maybeBusiness of
    Nothing -> return (businessKeyId, Nothing)
    Just business -> do
      let businessUpdated -> do
      let businessUpdated = Business {
-- JSON might not have the updated value so store down a default value
-- fromMaybe (defaultValue) (optimisticValue) >> Format for fromMaybe function
businessName = fromMaybe (businessName business) (businessJSONName businessJSON)
, businessEmail = fromMaybe (businessEmail business) (businessJSONEmail businessJSON)
}

-- Update the business name and email in the database
withDbRun $ DbSql.update businessKeyId [
  BusinessName =. businessName businessUpdated
  , BusinessEmail =. businessEmail businessUpdated
                                       ]
  return (businessKeyId, Just businessUpdated)

deleteBusinessById maybeIdBS = do
  let businessIdKey = getBusinessIdKey maybeIdBS
  -- Look up the business in the database
  (businessKeyId, maybeBusiness) <- getBusinessById maybeIdBS
  case maybeBusiness of
    Nothing -> return (businessKeyId, Nothing)
    -- Business?
    Just business -> do
      -- delete the business from the DB
      withDbRun $ DbSql.delete businessKeyId
      return (businessKeyId, Just business)
