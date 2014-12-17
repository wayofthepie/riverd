{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
{-
data Project = Project
    { projectName :: !Text
    , repoUrl     :: !Text
    , jenkinsInfo :: !Text
    } deriving (Eq, Show)


data JenkinsInfo = JenkinsInfo
    { url   :: !Text  -- | Base url of jenkins
    , jobs  :: [Text] -- | List of job urls
    } deriving (Eq, Show)
-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    projectName Text
    repoUrl     Text
    jenkinsInfo Text
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    testId  <- insert $ Project "Test" "github.com" "10.64.46.1"
    test    <- get testId
    liftIO $ print test
