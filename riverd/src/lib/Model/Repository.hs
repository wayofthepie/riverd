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
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle hiding (Schema)



share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name        String
    extDeps     [ExternalDependency]
    buildSteps  [String]
    UniqueName  name
    deriving Eq Generic Read Typeable Show

ExternalDependency
    name        String
    version     String
    description String
    NameVersion name version
    deriving Eq Generic Read Typeable Show
|]


-- | Project.
-- Configuration for rest-core
deriveAll ''Project "PFProject"

type instance PF Project = PFProject
instance FromJSON Project where parseJSON = gparseJson
instance ToJSON   Project where toJSON    = gtoJson

instance JSONSchema Project where
    schema  _ =
        let jsArrayTGen t = JS.Array unboundedLength False t
            nameF        = field "projectName" True value
            extDepsF     = field "projectExtDeps" False $
                jsArrayTGen (gSchema (Proxy :: Proxy ExternalDependency))

            buildStepsF  = field "projectBuildSteps" False $
                jsArrayTGen value
        in  merge buildStepsF $ merge nameF extDepsF

instance XmlPickler Project where
    xpickle =
        let toProject   = \(a, ts, ss) -> Project a ts ss
            fromProject = \(Project a ts ss) -> (a, ts, ss)
        in  xpElem "project" $
            xpWrap ( toProject , fromProject ) $
                xpTriple (xpElem "name" xpText)
                    (xpElem "dependencies" xpickle)
                    (xpElem "steps" $ xpList (xpElem "step" xpText))

-------------------------------------------------------------------------------
-- | ExternalDependency
deriveAll ''ExternalDependency "PFExternalDependency"

type instance PF ExternalDependency = PFExternalDependency
instance FromJSON ExternalDependency where parseJSON = gparseJson
instance ToJSON   ExternalDependency where toJSON    = gtoJson
instance JSONSchema ExternalDependency where schema  = gSchema

-- | NOTE: The generic instance of XmlPickler for ExternalDependency (i.e
-- using gxpickle) creates an instance that has an infinite loop. Would be
-- worthwhile investigating and raising an issue.
instance XmlPickler ExternalDependency where
    xpickle =
        let toExtDep   = \(a, ts, ss) -> ExternalDependency a ts ss
            fromExtDep = \(ExternalDependency a ts ss) -> (a, ts, ss)
        in xpElem "dependency" $
           xpWrap (toExtDep, fromExtDep) $
                xpTriple (xpElem "name" xpText)
                         (xpElem "version" xpText)
                         (xpElem "description" xpText)



-- | Temporary function to run a quick store test
test :: IO ()
test = runSqlite ":memory:" $ do
    runMigration migrateAll
    projectId <- insert $
        Project "test"
                [ExternalDependency "gradle" "2.2.0" "Gradle!"]
                ["gradle clean build"]
    project <- get projectId
    liftIO $ print project

