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
import Happstack.Server
import Rest.Driver.Happstack

import Api
import Api.Types.RiverdApi

import Debug.Trace

main :: IO ()
main = getConfig >>=
    \c -> simpleHTTP (Conf 8000 Nothing Nothing 60 Nothing) (handle c) >>
        return ()

handle :: Config -> ServerPartT IO Response
handle config = traceShow "Handle ..." $
    apiToHandler' (liftIO . runRiverdApi config) api

