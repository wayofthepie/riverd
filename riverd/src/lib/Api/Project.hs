{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Project where

import Control.Monad.Error (ErrorT)
import Control.Monad.Reader
import Data.Aeson
import Data.JSON.Schema as JS hiding (content)
import Data.JSON.Schema.Combinators
    (field, merge, value)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Rest
import qualified Rest.Resource as R
import Text.XML.HXT.Arrow.Pickle hiding (Schema)


data Project = Project
    { title     :: String
    , content   :: [String]
    } deriving (Generic, Typeable, Show)

deriveAll ''Project "PFProject"

type instance PF Project = PFProject
instance FromJSON   Project where parseJSON = gparseJson
instance ToJSON     Project where toJSON    = gtoJson


instance JSONSchema Project where
    schema  _ =
        let contentF    = field "content" False (JS.Array unboundedLength False value)
            titleF      = field "title" True value
        in merge contentF titleF


instance XmlPickler Project where
    xpickle = xpElem "project" $
        xpWrap ( uncurry Project, \(Project a bs) -> (a, bs) ) $
            xpPair (xpElem "title" xpText) (xpElem "list" $ xpList (xpElem "item" xpText))


resource :: Resource IO (ReaderT String IO) String () Void
resource = mkResourceReader
    { R.name    = "project"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const listProjects
    , R.get     = Just getProject
    }


getProject :: Handler (ReaderT String IO)
getProject = mkIdHandler xmlJsonO $ \_ titleStr -> liftIO $ readProject titleStr


readProject :: String -> IO Project
readProject t = return Project { title = t, content = ["Test!", "Hey!"] }


listProjects :: ListHandler IO
listProjects = mkListing xmlJsonO handler
    where
        handler :: Range -> ErrorT (Reason ()) IO [Project]
        handler r = liftIO $ readProjects (offset r) (count r)


readProjects :: Int -> Int -> IO [Project]
readProjects _ _ =
    return
        [
            Project { title = "Project One", content = ["First."] },
            Project { title = "Project Two", content = ["Second!"] }
        ]
