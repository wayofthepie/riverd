{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StashClient where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe
import GHC.Generics
import Network.HTTP.Conduit

import qualified Stash.Types.Project as P
import qualified Stash.Types.Repo as R

data PagedResponse a = PagedResponse
    { size      :: Int
    , limit     :: Int
    , isLastPage:: Bool
    , values    :: a
    } deriving (Eq, Generic, Show)

instance FromJSON (PagedResponse R.Repo)
instance FromJSON (PagedResponse P.Project)


-- | apiEndpointUrl hostname apiVersion query path
-- Hardcoded to use http for testing purposes
apiEndpointUrl :: String -> String -> String -> Maybe String -> String
apiEndpointUrl h v p q
    | isJust q  = url ++ fromJust q
    | otherwise = url
    where url =concat ["http://", h, "/rest/api/", v, "/", p, "?"]


requestBuilder :: ( MonadIO m, MonadThrow m ) =>
    (Request -> Request) -> String -> (String, String) -> m Request

requestBuilder httpMethod ep (user, pass) =
    parseUrl ep >>= \url -> return $ addBasicAuth . acceptJson . httpMethod $ url
    where
        addBasicAuth :: Request -> Request
        addBasicAuth req = applyBasicAuth (BC.pack user) (BC.pack pass) req

        acceptJson :: Request -> Request
        acceptJson req =
            req { requestHeaders = [("Accept", "application/json")] }


-- | getResponse request
getResponse ::
    ( MonadIO m, MonadBaseControl IO m ) => Request -> m ( Response BLC.ByteString )
getResponse req = withManager $ \manager -> do
    resp <- httpLbs req manager
    return resp


getProjects :: ( MonadBaseControl IO m, MonadIO m,  MonadThrow m ) =>
    m ( Either String ( PagedResponse P.Project ) )
getProjects = liftM ( decoder. responseBody ) $
    getResponse =<< requestBuilder modifyRequest endpoint ("chaospie","test")
    where
        endpoint :: String
        endpoint = apiEndpointUrl "192.168.11.15:7990" "1.0" "projects" Nothing

        modifyRequest :: Request -> Request
        modifyRequest req = req { method = "GET",responseTimeout=Just (1000000) }

        decoder :: BLC.ByteString -> Either String ( PagedResponse P.Project )
        decoder = eitherDecode
