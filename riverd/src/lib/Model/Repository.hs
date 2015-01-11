{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Repository where

import Control.Monad.IO.Class
import Data.Aeson
import Data.JSON.Schema as JS hiding (content)
import Data.JSON.Schema.Combinators (field, merge, value )
import Data.Typeable
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Text.XML.HXT.Arrow.Pickle hiding (Schema)



share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
Project
    title String
    content [String]
    deriving Eq Generic Read Typeable Show
|]


-- | Project.
-- Configuration for rest-core
deriveAll ''Project "PFProject"

type instance PF Project = PFProject
instance FromJSON   Project where parseJSON = gparseJson
instance ToJSON     Project where toJSON    = gtoJson

instance JSONSchema Project where
    schema  _ =
        let contentF    = field "projectContent" False (JS.Array unboundedLength False value)
            titleF      = field "projectTitle" True value
        in merge contentF titleF

instance XmlPickler Project where
    xpickle = xpElem "project" $
        xpWrap ( uncurry Project , \(Project a bs) -> (a, bs) ) $
            xpPair (xpElem "title" xpText) (xpElem "list" $ xpList (xpElem "item" xpText))


ptitle :: Project -> String
ptitle (Project t _) = t

pcontent :: Project -> [String]
pcontent (Project _ c) = c



test :: IO ()
test = runSqlite ":memory:" $ do
    runMigration migrateAll
    projectId <- insert $ Project "test" ["test1","test2"]
    project <- get projectId
    liftIO $ print project

