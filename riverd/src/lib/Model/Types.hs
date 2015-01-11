{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Types where

import Data.Aeson
import Data.JSON.Schema as JS hiding (content)
import Data.JSON.Schema.Combinators (field, merge, value )
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Text.XML.HXT.Arrow.Pickle hiding (Schema)


data Project = Project
    { title :: String
    , content :: [String]
    } deriving (Generic, Typeable, Show)

deriveAll ''Project "PFProject"

type instance PF Project = PFProject
instance FromJSON   Project where parseJSON = gparseJson
instance ToJSON     Project where toJSON    = gtoJson

instance JSONSchema Project where
    schema  _ =
        let contentF    = field "content" False (JS.Array unboundedLength False value)
            titleF      = field "title" True value
        in merge contentF titleF

instance XmlPickler Project where
    xpickle = xpElem "project" $
        xpWrap ( uncurry Project , \(Project a bs) -> (a, bs) ) $
            xpPair (xpElem "title" xpText) (xpElem "list" $ xpList (xpElem "item" xpText))
