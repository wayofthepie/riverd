{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Repo where

import Data.Aeson
import GHC.Generics

data Repo = Repo
    { slug          :: String
    , id            :: Int
    , name          :: String
    , state         :: String
    , statusMessage :: String
    , forkable      :: Bool
    , project       :: Object
    , public        :: Bool
    , link          :: Object
    , cloneUrl      :: String
    , links         :: Object
    } deriving (Eq, Generic, Show)

instance FromJSON Repo



