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

import Api.Types.RiverdApi
import Api.Types.Project



resource :: Resource RiverdApi (ReaderT String RiverdApi) String () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const listProjects
    , R.get     = Just getProject
    , R.create  = Just create
    }


getProject :: Handler (ReaderT String RiverdApi)
getProject = mkIdHandler xmlJsonO $ \_ titleStr -> liftIO $ readProject titleStr


readProject :: String -> IO Project
readProject t = return $ Project "test" ""


listProjects :: ListHandler RiverdApi
listProjects = mkListing xmlJsonO handler
    where
        handler :: Range -> ErrorT (Reason ()) RiverdApi [Project]
        handler r = liftIO $ readProjects (offset r) (count r)


readProjects :: Int -> Int -> IO [Project]
readProjects _ _ =
    return
        [
            Project "test" "",
            Project "test2" ""
        ]

create :: Handler RiverdApi
create = mkInputHandler ( xmlJsonI . xmlJsonO . xmlJsonE ) handler
    where
        handler :: Project -> ErrorT (Reason ProjectCreationError) RiverdApi Int
        handler p = maybe (return 200) throwError $ Just . domainReason $
                            ProjectAlreadyExists "Project exists"


-- | If for IO Bool
--if' b t f = if b then t else f
--ifM = liftM3 if'

