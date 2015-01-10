{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api
import Control.Monad.Trans
import Happstack.Server
import Rest.Driver.Happstack

main :: IO ()
main = simpleHTTP nullConf handle

handle :: ServerPartT IO Response
handle = apiToHandler' liftIO api
