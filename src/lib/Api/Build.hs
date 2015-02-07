{-# LANGUAGE
    DeriveDataTypeable
    , DeriveGeneric
    , EmptyDataDecls
    , OverloadedStrings
    , TemplateHaskell
    , TypeFamilies
    #-}

module Api.Build where

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
import Api.Types.Build
import qualified Model.Repository as Repo

import Debug.Trace

resource :: Resource RiverdApi (ReaderT String RiverdApi) String Void Void
resource = mkResourceReader
    { R.name    = "build"
--    , R.schema  = withListing () $ named [("name", singleBy id)]
--    , R.list    = const listProjects
--    , R.get     = Just getProject
    , R.create  = Just createBuild
    }


-- | Starts a new build
createBuild :: Handler RiverdApi
createBuild = mkInputHandler ( xmlJsonI . xmlJsonO . xmlJsonE ) handler
  where
    handler :: BuildSpec -> ErrorT (Reason BuildCreationError) RiverdApi Int
    handler bs = traceShow "Called" $ do
        pool <- asks pool
        err  <- liftIO $ do
            projectKey <- Repo.getProjectKeyByName pool $ projectName bs
            case projectKey of
                Just k  -> Repo.insertBuild pool (projectName bs) k >>
                        return Nothing
                Nothing -> return . Just $ domainReason $
                        ProjectDoesNotExist "Cannot create build."
        maybe (return 201) throwError err
