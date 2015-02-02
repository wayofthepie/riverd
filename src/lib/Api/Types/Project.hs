{-# LANGUAGE
    DeriveDataTypeable
    , DeriveGeneric
    , EmptyDataDecls
    , TemplateHaskell
    , TypeFamilies
    #-}


module Api.Types.Project where

import Data.Aeson
import Data.JSON.Schema hiding (content)
import Data.Text as T
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle hiding (Schema)


data Project = Project
    { name    :: T.Text
    , repoUrl :: T.Text
    } deriving (Eq, Generic, Ord, Show, Typeable)

data ProjectCreationError = ProjectAlreadyExists String
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Project "PFProject"
type instance PF Project = PFProject

instance FromJSON   Project
instance ToJSON     Project
instance JSONSchema Project where schema  = gSchema
instance XmlPickler Project where xpickle = gxpickle


deriveAll ''ProjectCreationError "PFProjectCreationError"
type instance PF ProjectCreationError = PFProjectCreationError

instance ToJSON     ProjectCreationError
instance FromJSON   ProjectCreationError
instance JSONSchema ProjectCreationError where schema = gSchema
instance XmlPickler ProjectCreationError where xpickle = gxpickle

instance ToResponseCode ProjectCreationError where
    toResponseCode _ = 418
