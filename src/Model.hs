{-# LANGUAGE
OverloadedStrings
  , EmptyDataDecls
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  , GADTs
  , TypeFamilies
  , TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , StandaloneDeriving #-}

-- Export `Business` record(model)
-- the entity (business) definition
-- and the entity fields' setters and getters

module Model (
       Business (..)
       , entityDefs
       , EntityField(..)
       ) where

-- needed for encoding and decoding to/from JSON
import GHC.Generics
import Data.Aeson
import Data.Default.Class


-- Needed from generating business entity

import Database.Persist
import Database.Persist.Class
import Database.Persist.TH


-- Generates our `Business` instances and Business record

share [mkPersist sqlSettings, mkSave "entityDefs"][persistLowerCase|
 Business
 name String
 email String
 deriving Show Generic
 |]


-- Defines the ToJSON interface with `Business` record
-- This will take a `Business` record and convert it to JSON
-- For Example:
  -- > let x = Business {businessName = "Test Name", businessEmail ="test@business.com"}
  -- > toJSON x
  -- Onject (fromList [("email", String "test@business.com"), ("name", String "Test Name")])
   -- > encode $ toJSON x
  -- "{\"email\": \"test@business.com\", \"name\": \"Test Business\"\}"

  instance ToJSON Business where
toJSON (Business name email) = object ["name" .= name, "email" .= email]
