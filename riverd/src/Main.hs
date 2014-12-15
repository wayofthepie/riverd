{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid ((<>), mempty)
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, dir, seeOther)
import System.FilePath
import Data.Text
import qualified Data.Text as Text          -- text
import qualified Data.Text.IO as Text       -- text
import           Jenkins.Discover           -- libjenkins
import           System.Exit (exitFailure)  -- base

main :: IO ()
main = print "test" {-simpleHTTP nullConf $ msum
    [ dir "hello"    $ ok "Hello, World!"
    , dir "goodbye"  $ ok "Goodbye, World!"
    , seeOther "/hello" "/hello"
    ]-}
{-
test :: RepositoryFactory n m r 
test = do
    repo <- openRepository repoOpts False
    gr repo    

repoOpts = RepositoryOptions { repoPath = "/opt/repos/riverd"
                                     , repoWorkingDir = Nothing
                                     , repoIsBare = False
                                     , repoAutoCreate = False
                                     }
-}


disc = do
  -- send discover broadcast with 100ms timeout
  discoveries <- discover 100000
  case discoveries of
    -- no Jenkins responded
    [] -> exitFailure
    -- pretty print responses
    _  -> mapM_ (Text.putStrLn . pretty) discoveries

-- | Pretty print Jenkins discovery responses
pretty :: Discover -> Text
pretty x = Text.unwords $
  "Jenkins" : version x : maybe mempty (return . between "(" ")") (serverId x) ++ ["at", url x]
 where
  between l r t = l <> t <> r
