{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module RiverdSpec
    ( RiverdSpec
    , Build
    , readSpec
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics

data RiverdSpec = RiverdSpec
    { projectName   :: Text
    , repoUrl       :: Text
    , branchSpec    :: Maybe Text
    , builds        :: [Build]
    } deriving (Eq, Generic, Show)

rSpecProjectName    = projectName
rSpecRepoUrl        = repoUrl
rSpecBranchSpec     = branchSpec
rSpecBuilds         = builds


data Build = Build
    { buildSystem  :: Text
    , buildParams  :: [Text]
    } deriving (Eq, Generic, Show)

rSpecBuildSystem    = buildSystem
rSpecBuildParams    = buildParams


instance FromJSON RiverdSpec
instance ToJSON RiverdSpec

instance FromJSON Build
instance ToJSON Build

-- | Reads a file into a bytestring
file2bs :: FilePath -> IO B.ByteString
file2bs f = B.readFile f

-- | Reads a spec file into a RiverdSpec
readSpec :: FilePath ->  IO (Either String RiverdSpec)
readSpec f = do
    d <- (eitherDecode <$> file2bs f) :: IO (Either String RiverdSpec)
    return d

