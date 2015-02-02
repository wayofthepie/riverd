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
import qualified Data.Text as T
import Data.Typeable
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name        T.Text
    repoUrl     T.Text
    UniqueName  name
    deriving Eq Generic Read Typeable Show
|]


runSql :: ConnectionPool -> SqlPersistM a -> IO a
runSql pool sql = runSqlPersistMPool sql pool


-- | insertProject name repoUrl
insertProject :: ConnectionPool -> T.Text -> T.Text -> IO (Key Project)
insertProject pool n url = runSql pool $ insert $ Project n url


doesProjectExist :: ConnectionPool -> T.Text -> IO Bool
doesProjectExist pool n = do
    maybeProject <- runSql pool $ getBy $ UniqueName n
    case maybeProject of
        Just _ -> return $ True
        Nothing -> return $ False

{-
createProject = do
oool :: ( MonadBaseControl IO m, MonadIO m, MonadLogger IO ) => m ConnectionPool
    (insert:: PersistEntity e => e -> m (Key e))  $ Project "" [ExternalDependency "test" "test" "test"] ["test"]
-}
