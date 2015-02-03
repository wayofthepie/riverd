{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Project where

import Control.Monad.Error (runErrorT, throwError, ErrorT)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.Text as T
import Database.Persist.Class hiding (count)
import qualified Database.Persist.Sql as DB
import Rest
import Rest.Error
import qualified Rest.Resource as R

import Api.Types.RiverdApi
import Api.Types.Project
import qualified Model.Repository as Repo


import Debug.Trace

resource :: Resource RiverdApi (ReaderT String RiverdApi) String () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("name", singleBy id)]
    , R.list    = const listProjects
    , R.get     = Just getProject
    , R.create  = Just create
    }


getProject :: Handler (ReaderT String RiverdApi)
getProject = mkIdHandler xmlJsonO handler
  where
    handler :: () -> String -> ErrorT (Reason ()) (ReaderT String RiverdApi) (Maybe Project)
    handler _ name = do
        pool <- lift . lift $ asks pool
        project <- liftIO $ do
            readProject pool name
        return project


readProject :: DB.ConnectionPool -> String -> IO (Maybe Project)
readProject pool name =
    liftM maybeProject $ Repo.getProjectByName pool (T.pack name)
  where
    maybeProject :: Maybe Repo.Project -> Maybe Project
    maybeProject (Just (Repo.Project n u)) = Just $ Project n u
    maybeProject Nothing                   = Nothing


-- | Return a paged list of projects.
listProjects :: ListHandler RiverdApi
listProjects = mkListing xmlJsonO handler
  where
    handler :: Range -> ErrorT (Reason ()) RiverdApi [Project]
    handler r = do
        pool <- asks pool
        projects <- liftIO $ do
            let pageOffset = Repo.consOffset $ offset r
                pageLimit  = Repo.consLimit  $ count r
            readProjects pool pageOffset pageLimit
        return projects


-- | Read projects from DB.
readProjects :: DB.ConnectionPool -> Repo.Offset -> Repo.Limit -> IO [Project]
readProjects pool off lim =
    liftM (map (\(Repo.Project n u) -> Project n u)) $
        Repo.readProjects pool off lim


-- | Create a project.
create :: Handler RiverdApi
create = mkInputHandler ( xmlJsonI . xmlJsonO . xmlJsonE ) handler
  where
    handler :: Project -> ErrorT (Reason ProjectCreationError) RiverdApi Int
    handler p = do
        pool <- asks pool
        err <- liftIO $ do
            e <- Repo.doesProjectExist pool (name p)
            if not e
                then do
                    Repo.insertProject pool (name p) (repoUrl p) >>
                        return Nothing
                else do
                    return . Just $ domainReason $
                        ProjectAlreadyExists "Project exists"
        maybe (return 201) throwError err


-- | If for IO Bool
--if' b t f = if b then t else f
--ifM = liftM3 if'

