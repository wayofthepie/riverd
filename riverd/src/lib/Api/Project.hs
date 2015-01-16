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
import qualified Database.Persist.Sql as DB
import Rest
import Rest.Error
import qualified Rest.Resource as R

import Model.Types
import Model.ProjectCreationError
import Model.Repository




resource :: Resource IO (ReaderT String IO) String () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const listProjects
    , R.get     = Just getProject
    , R.create  = Just create
    }


getProject :: Handler (ReaderT String IO)
getProject = mkIdHandler xmlJsonO $ \_ titleStr -> liftIO $ readProject titleStr


readProject :: String -> IO Project
readProject t = return $ Project "test"
                    []
                    ["gradle clean build"]


listProjects :: ListHandler IO
listProjects = mkListing xmlJsonO handler
    where
        handler :: Range -> ErrorT (Reason ()) IO [Project]
        handler r = liftIO $ readProjects (offset r) (count r)


readProjects :: Int -> Int -> IO [Project]
readProjects _ _ =
    return
        [
            Project "test"
                []
                ["gradle clean build"],
            Project "test2"
                []
                ["gradle clean build"]
        ]

create :: Handler IO
create = mkInputHandler ( xmlJsonI . xmlJsonO . xmlJsonE ) handler
    where
        doesProjectExist :: Project -> IO (Bool)
        doesProjectExist p =
            let search   = runSql . DB.getBy . UniqueName $ projectName p
                verify m | isNothing m = False
                         | otherwise   = True
            in  liftM verify $ search

        insertProject :: Project -> IO (DB.Key Project)
        insertProject p = runSql $ DB.insert p

        handler :: Project -> ErrorT (Reason ProjectCreationError) IO Int
        handler p = do
            rv <- liftIO $ do
                pe <- doesProjectExist p
                if pe
                    then do
                        return . Just . domainReason $
                            ProjectAlreadyExists "Project exists"
                    else
                        insertProject p >> return Nothing
            maybe (return 200) throwError rv

-- | If for IO Bool
--if' b t f = if b then t else f
--ifM = liftM3 if'


