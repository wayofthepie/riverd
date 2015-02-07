{-# LANGUAGE
    DeriveDataTypeable
    , DeriveGeneric
    , FlexibleContexts
    , FlexibleInstances
    , GADTs
    , MultiParamTypeClasses
    , OverloadedStrings
    , QuasiQuotes
    , Rank2Types
    , TemplateHaskell
    , TypeFamilies
    #-}

module Model.Repository where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
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
Build
    buildName       T.Text
    projectId       ProjectId
    UniqueBuildName buildName
|]


runSql :: MonadBaseControl IO m => ConnectionPool -> SqlPersistT m a -> m a
runSql pool sql = runSqlPool sql pool

-------------------------------------------------------------------------------
-- Project functions

-- | insertProject name repoUrl
insertProject :: ConnectionPool -> T.Text -> T.Text -> IO (Key Project)
insertProject pool n url = runSql pool $ insert $ Project n url


doesProjectExist :: ConnectionPool -> T.Text -> IO Bool
doesProjectExist pool n = do
    maybeProject <- getProjectByName pool n
    case maybeProject of
        Just _ -> return $ True
        Nothing -> return $ False


-- This is how much I enjoy type safety ...
newtype Offset = Offset Int
consOffset :: Int -> Offset
consOffset = Offset

newtype Limit = Limit Int
consLimit :: Int -> Limit
consLimit = Limit


readProjects :: ConnectionPool -> Offset -> Limit -> IO [Project]
readProjects pool (Offset off) (Limit lim) = do
    liftM (map entityVal) $ runSql pool $ selectList []
        [ Asc ProjectName
        , OffsetBy off
        , LimitTo lim ]


getProjectByName :: ConnectionPool -> T.Text -> IO (Maybe Project)
getProjectByName pool n = do
    maybeProject  <- runSql pool $ getBy $ UniqueName n
    case maybeProject of
        Just p  -> return . Just $ entityVal p
        Nothing -> return  Nothing

getProjectKeyByName :: ConnectionPool -> T.Text -> IO (Maybe (Key Project))
getProjectKeyByName pool n = do
    maybeProject  <- runSql pool $ getBy $ UniqueName n
    case maybeProject of
        Just p  -> return . Just $ entityKey p
        Nothing -> return  Nothing


-------------------------------------------------------------------------------
-- Build functions
insertBuild :: ConnectionPool -> T.Text -> Key Project -> IO (Key Build)
insertBuild pool bname projectId = runSql pool $ insert $ Build bname projectId
