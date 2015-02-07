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

resource :: Resource RiverdApi (ReaderT String RiverdApi) String () Void
resource = mkResourceReader
    { R.name    = "build"
    , R.schema  = withListing () $ unnamedSingle (\s -> s)
    , R.list    = const listBuildConfigs
--    , R.get     = Just getBuildConfig
    , R.create  = Just createBuild
    }


listBuildConfigs :: ListHandler RiverdApi
listBuildConfigs = mkListing xmlJsonO handler
  where
    handler :: Range -> ErrorT (Reason ()) RiverdApi [BuildSpec]
    handler r = do
        pool         <- asks pool
        buildConfigs <- liftIO $ do
            let pageOffset = Repo.consOffset $ offset r
                pageLimit  = Repo.consLimit  $ count r
            readBuildConfigs pool pageOffset pageLimit
        return buildConfigs


-- |
-- FIXME : Once build steps are passed and saved the mapping between a Build and
-- a BuildSpec here must be updated.
readBuildConfigs :: DB.ConnectionPool -> Repo.Offset -> Repo.Limit -> IO [BuildSpec]
readBuildConfigs pool off limit =
    liftM (map (\(Repo.Build n _) -> BuildSpec n "" [])) $
        Repo.readBuildConfigs pool off limit



-- | Starts a new build
createBuild :: Handler RiverdApi
createBuild = mkInputHandler ( xmlJsonI . xmlJsonO . xmlJsonE ) handler
  where
    handler :: BuildSpec -> ErrorT (Reason BuildCreationError) RiverdApi Int
    handler bs = traceShow "Called" $ do
        pool <- asks pool
        err  <- liftIO $ do
            projectKey <- traceShow "PKEY" $ Repo.getProjectKeyByName pool $ projectName bs
            case projectKey of
                Just k  -> Repo.insertBuild pool (projectName bs) k >>= \e ->
                        return $ maybeError e
                Nothing -> return . Just $ domainReason $
                        ProjectDoesNotExist "Cannot create build."
        maybe (return 201) throwError err

    maybeError :: Either String (Key Repo.Build) -> Maybe (Reason BuildCreationError)
    maybeError e = case e of
        Left s  -> Just $ domainReason $ BuildCreationError s
        Right _ -> Nothing
