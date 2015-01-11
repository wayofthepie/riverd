{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Project where

import Control.Monad.Error (ErrorT)
import Control.Monad.Reader
import Rest
import qualified Rest.Resource as R

import Model.Types

resource :: Resource IO (ReaderT String IO) String () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const listProjects
    , R.get     = Just getProject
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

