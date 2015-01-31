{-# LANGUAGE
    DeriveDataTypeable
    , DeriveGeneric
    , FlexibleContexts
    , FlexibleInstances
    , GADTs
    , MultiParamTypeClasses
    , OverloadedStrings
    , QuasiQuotes
    , TemplateHaskell
    , TypeFamilies
    #-}

module Model.Repository where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Typeable
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name        String
    repoUrl     String
    UniqueName  name
    deriving Eq Generic Read Typeable Show
|]

createConnPool = runStdoutLoggingT $ createSqlitePool ":test:" 10

runSql :: SqlPersistM a -> IO a
runSql sql = runSqlPersistMPool sql =<< createConnPool



{-
createProject = do
oool :: ( MonadBaseControl IO m, MonadIO m, MonadLogger IO ) => m ConnectionPool
    (insert:: PersistEntity e => e -> m (Key e))  $ Project "" [ExternalDependency "test" "test" "test"] ["test"]
-}
