
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Links where

import Data.Aeson
import GHC.Generics

data Links = Links
    { clone :: Maybe String
    , self  :: [Href]
    } deriving (Eq, Generic, Show)

data Href = Href { href :: String }
    deriving (Eq, Generic, Show)

instance FromJSON Links
instance FromJSON Href
