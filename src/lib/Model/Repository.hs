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

import Control.Exception
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

import Debug.Trace

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name        T.Text
    repoUrl     T.Text
    UniqueName  name
BuildConfig
    bcName       T.Text
    projectId    ProjectId
    UniqueBCName bcName
|]


runSql :: MonadBaseControl IO m => ConnectionPool -> SqlPersistT m a -> m a
runSql pool sql = runSqlPool sql pool

-------------------------------------------------------------------------------
-- Project functions

-- | insertProject name repoUrl
insertProject ::
    ConnectionPool -> T.Text -> T.Text -> IO (Either String (Key Project))
insertProject pool n url = do
    e <- traceShow "Does project exist ... " $ doesProjectExist pool n
    case e of
        True  -> return $ Left "Error, project with name"
        False -> traceShow "Inserting ... " $ insertProject' pool n url


insertProject' ::
    ConnectionPool -> T.Text -> T.Text -> IO (Either String (Key Project))
insertProject' pool n url = do
    e <- try (runSql pool $ insert $ Project n url) :: IO (Either PersistentSqlException (Key Project))
    case e of
        Right a -> return $ Right a
        Left ex -> return $ Left "An exception of "


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
-- BuildConfig functions

-- |
-- FIXME: Is it right to just return a string error message if any
-- exception occurs...?
insertBuildConfig :: ConnectionPool -> T.Text -> Key Project -> IO (Either String (Key BuildConfig))
insertBuildConfig pool bname projectId = do
    maybeBuildConfig <- doesBuildConfigExist pool bname
    case maybeBuildConfig of
        True  -> return $ Left "A build already exists."
        False -> insertBuildConfig' pool $ BuildConfig bname projectId

insertBuildConfig' ::
    ConnectionPool -> BuildConfig -> IO (Either String (Key BuildConfig))
insertBuildConfig' pool b = do
    e <- traceShow "inserting b"$ try (runSql pool $ insert b) :: IO (Either PersistentSqlException (Key BuildConfig))
    case e of
        Right a -> return $ Right a
        Left ex -> return $ Left $ "An exception of " ++ show ex

doesBuildConfigExist :: ConnectionPool -> T.Text -> IO Bool
doesBuildConfigExist pool n = do
    maybeBuildConfig <- runSql pool $ getBy $ UniqueBCName n
    case maybeBuildConfig of
        Just b  -> return True
        Nothing -> return False

readBuildConfigs :: ConnectionPool -> Offset -> Limit -> IO [BuildConfig]
readBuildConfigs pool (Offset off) (Limit lim) = do
    liftM (map entityVal) $ runSql pool $ selectList []
        [ Asc BuildConfigBcName
        , OffsetBy off
        , LimitTo lim ]


