{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Project where

import Control.Applicative
import Data.Aeson


data Project = Project
    { key   :: String
    , id    :: Int
    , name  :: String
    , public:: Bool
    , typ   :: String
    , link  :: Object
    , links :: Object
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON (Object x) = Project
        <$> x .: "key"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "public"
        <*> x .: "type"
        <*> x .: "link"
        <*> x .: "links"
    parseJSON _ = fail "Expected an object!"
