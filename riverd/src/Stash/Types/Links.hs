
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Links where

import Data.Aeson
import GHC.Generics

data Links = Links
    { clone :: Maybe String
    , self  :: String
    } deriving (Eq, Generic, Show)

instance FromJSON Links
