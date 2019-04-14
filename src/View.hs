{-# LANGUAGE
OverloadedStrings
, DeriveGeneric #-}

module View (
            BusinessJSON
            , businessJSONTOBusiness
            , businessAsJSONLBS -- LBS stands for Lazy Byte String
            ) where

-- Our custom Model module

import Model

-- Build Dependencies

import GHC.Generics
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Data.Default.Class
import Database.Persist
import Database.Persist.Class

-- Our "view" or "BusinessJSON" record

data BusinessJSON = BusinessJSON {
    businessJSONName :: Maybe String
  , businessJSONEmail :: Maybe String
  } deriving (Show, Generic)


-- Defining how to parse JSON string
-- into BusinessJSON record

instance FromJSON BusinessJSON where
  parseJSON (Object v) =
    BusinessJSON <$> v .:? "name" -- .:? is syntax for parsing a JSON string field into a Maybe String
                 <*> v .:? "email" -- if JSON field doesn't exist email will be `Nothing`



instance toJSON BusinessJSON
  toJSON (BusinessJSON name email) = object ["name" .= name, "email" .= email]

businessJSONTOBusiness :: BusinessJSON -> Business
businessJSONTOBusiness businessJSON = Business nameJSONToName emailJSONToEmail
  where
    -- if name is Nothing default to empty string
    nameJSONToName = fromMaybe "" $ businessJSONName businessJSON
    -- if email is Nothing default to empty string
    emailJSONToEmail = fromMaybe "" $ businessJSONEmail businessJSON

businessAsJSONLBS :: Key Business -> Business -> Data.ByteString.Lazy.ByteString
businessAsJSONLBS k b = encode . entifyIdToJSON $ Entity k b
