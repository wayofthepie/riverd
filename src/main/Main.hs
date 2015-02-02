{-# LANGUAGE
    OverloadedStrings
    , GeneralizedNewtypeDeriving
    #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Trans
import Database.Persist
import Database.Persist.Sqlite
import Happstack.Server
import Rest.Driver.Happstack

import Api
import Api.Types.RiverdApi
import Model.Repository as Repo
import Debug.Trace

main :: IO ()
main = do
    c <- getConfig
    let connPool = pool c
    runSql connPool $ runMigration migrateAll
    simpleHTTP (Conf 8000 Nothing Nothing 60 Nothing) (handle c)

handle :: Config -> ServerPartT IO Response
handle config = traceShow "Handle ..." $
    apiToHandler' (liftIO . runRiverdApi config) api

