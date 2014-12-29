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
    } deriving (Eq, Show)


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
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response BLC.ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp


getProjects :: StashClient ( Either String ( PR.PagedResponse [P.Project] ) )
getProjects = do
    config <- ask
    liftM ( decoder . responseBody ) $
        getResponse =<< requestBuilder modifyRequest (endpoint config) (username config, password config)
    where
        endpoint :: StashClientConfig -> String
        endpoint config = apiEndpointUrl config "projects" Nothing

        modifyRequest :: Request -> Request
        modifyRequest req = req { method = "GET" }

        decoder :: BLC.ByteString -> Either String ( PR.PagedResponse [P.Project] )
        decoder = eitherDecode

