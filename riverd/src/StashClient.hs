{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StashClient where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, runReaderT, ReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe
import GHC.Generics
import Network.HTTP.Conduit

import qualified Stash.Types.Link           as Link
import qualified Stash.Types.Links          as Links
import qualified Stash.Types.PagedResponse  as PR
import qualified Stash.Types.Project        as P
import qualified Stash.Types.Repo           as R


data StashClientConfig = StashClientConfig
    { apiBase       :: String
    , apiVersion    :: String
    , apiProtocol   :: String
    , username      :: String
    , password      :: String
    , scManager     :: Manager
    }


defaultStashClientConfig :: ( MonadBaseControl IO m,  MonadIO m ) =>
    String -> String -> String -> m StashClientConfig
defaultStashClientConfig base user pass  =
    withManager $ \manager -> do
        return StashClientConfig { apiBase      = base
                                 , apiVersion   = "1.0"
                                 , apiProtocol  = "http://"
                                 , username     = user
                                 , password     = pass
                                 , scManager    = manager
                                 }


type StashClient a = ReaderT StashClientConfig (ResourceT IO) a


-- | Example
--
-- @
--  liftM (\\pr -> case pr of { Right x -> PR.values x; Left _ -> error ""})
--      runStashClient cfg $ getProjects
-- @
runStashClient :: MonadIO m => StashClientConfig -> StashClient a -> m a
runStashClient config action =
    liftIO $ runResourceT $ runReaderT action config


-- | apiEndpointUrl hostname apiVersion query path
-- Hardcoded to use http for testing purposes
apiEndpointUrl :: StashClientConfig -> String -> Maybe String -> String
apiEndpointUrl config path q
    | isJust q  = url ++ "?" ++ fromJust q
    | otherwise = url
    where url = concat [ apiProtocol config, apiBase config, "/rest/api/", apiVersion config, "/", path]


requestBuilder :: ( MonadIO m, MonadThrow m ) =>
    (Request -> Request) -> String -> (String, String) -> m Request
requestBuilder httpMethod ep (user, pass) =
    parseUrl ep >>= \url -> return $ addBasicAuth . acceptJson . httpMethod $ url
    where
        addBasicAuth :: Request -> Request
        addBasicAuth req = applyBasicAuth (BC.pack user) (BC.pack pass) req

        acceptJson :: Request -> Request
        acceptJson req =
            req { requestHeaders    = [("Accept", "application/json")]
                , responseTimeout   = Just 1000000
                }


-- | getResponse request
getResponse :: ( MonadIO m, MonadBaseControl IO m ) =>
    Manager -> Request -> m ( Response BLC.ByteString )
getResponse manager req = httpLbs req manager


reqEP :: String -> Maybe String ->
    (Request -> Request) -> StashClient ( Response BLC.ByteString )
reqEP ep q httpMethod = do
    config <- ask
    getResponse (scManager config) =<< requestBuilder httpMethod (endpoint config ep q)
            (username config, password config)
    where
        endpoint :: StashClientConfig -> String -> Maybe String -> String
        endpoint config ep q = apiEndpointUrl config ep q


mod2get :: (Request -> Request)
mod2get = \r -> r { method = "GET" }


getProjects :: StashClient ( Either String ( PR.PagedResponse [P.Project] ) )
getProjects = reqEP "projects" Nothing mod2get >>= return . eitherDecode . responseBody


getRepos :: String -> StashClient ( Either String ( PR.PagedResponse [R.Repo] ) )
getRepos pkey = reqEP ep Nothing mod2get >>= return . eitherDecode . responseBody
    where ep = "projects" ++ "/" ++ pkey ++ "/" ++ "repos"

