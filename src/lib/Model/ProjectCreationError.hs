{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , EmptyDataDecls
  , TemplateHaskell
  , TypeFamilies
  #-}
module Model.ProjectCreationError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data ProjectCreationError = ProjectAlreadyExists String
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''ProjectCreationError "PFProjectCreationError"
type instance PF ProjectCreationError = PFProjectCreationError

instance XmlPickler ProjectCreationError where xpickle = gxpickle
instance JSONSchema ProjectCreationError where schema = gSchema
instance FromJSON   ProjectCreationError
instance ToJSON     ProjectCreationError

instance ToResponseCode ProjectCreationError where
  toResponseCode _ = 418
