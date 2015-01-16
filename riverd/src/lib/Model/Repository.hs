{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Repository where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.Sqlite

import Model.Types

createConnPool = runStdoutLoggingT $ createSqlitePool ":test:" 10

runSql :: SqlPersistM a -> IO a
runSql sql = runSqlPersistMPool sql =<< createConnPool



{-
createProject = do
oool :: ( MonadBaseControl IO m, MonadIO m, MonadLogger IO ) => m ConnectionPool
    (insert:: PersistEntity e => e -> m (Key e))  $ Project "" [ExternalDependency "test" "test" "test"] ["test"]
-}
