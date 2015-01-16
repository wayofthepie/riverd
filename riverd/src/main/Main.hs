{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api
import Control.Monad.Trans
import Happstack.Server
import Rest.Driver.Happstack

main :: IO ()
main = simpleHTTP (Conf 8000 Nothing Nothing 60 Nothing) handle

handle :: ServerPartT IO Response
handle = apiToHandler' liftIO api
