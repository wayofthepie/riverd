{-# LANGUAGE
    GeneralizedNewtypeDeriving
    , OverloadedStrings
    #-}

module Api.Types.RiverdApi where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Database.Persist as DB
import qualified Database.Persist.Sqlite as DB
import System.Environment hiding (getEnvironment)


newtype RiverdApi a = RiverdApi { unRiverdApi :: ReaderT Config IO a }
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader Config
             )

runRiverdApi :: Config -> RiverdApi a -> IO a
runRiverdApi config = flip runReaderT config . unRiverdApi


data Config = Config
    { environment :: Environment
    , pool :: DB.ConnectionPool
    }


data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Read, Show)


getConfig :: IO Config
getConfig = do
    e <- getEnvironment
    p <- getPool e
    return Config
        { environment = e
        , pool = p
        }


-- | Retrieve the environment we are running in. This is read from the
-- environment variable RIVERD_ENV.
getEnvironment :: IO Environment
getEnvironment =
    (maybe Development read) <$> lookupEnv "RIVERD_ENV"


getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
    runStdoutLoggingT $ DB.createSqlitePool ":test:" 5


