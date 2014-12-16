{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Project (resource) where

import Control.Monad.Reader
import Data.Aeson
import Data.JSON.Schema hiding (content)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest
import qualified Rest.Resource as R
import Text.XML.HXT.Arrow.Pickle

type Title = String

data Project = Project
    { title :: Title
    , content :: String
    } deriving (Generic, Typeable, Show)

deriveAll ''Project "PFProject"

type instance PF Project = PFProject
instance FromJSON   Project where parseJSON = gparseJson
instance ToJSON     Project where toJSON    = gtoJson
instance JSONSchema Project where schema    = gSchema
instance XmlPickler Project where xpickle   = gxpickle

resource :: Resource IO (ReaderT Title IO) Title () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const list
    , R.get     = Just get
    }

get :: Handler (ReaderT Title IO)
get = mkIdHandler xmlJsonO $ \_ title -> liftIO $ readProject title

readProject :: Title -> IO Project
readProject t = return $ Project { title = t, content = "Hey!" }

list :: ListHandler IO
list = mkListing xmlJsonO $ \range -> lift $ readProjects (offset range) (count range)

readProjects :: Int -> Int -> IO [Project]
readProjects _ _ = return
    [
        Project { title = "Project One", content = "First." },
        Project { title = "Project Two", content = "Second!" }
    ]


