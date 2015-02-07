{-# LANGUAGE
    DeriveDataTypeable
    , DeriveGeneric
    , EmptyDataDecls
    , TemplateHaskell
    , TypeFamilies
    #-}


module Api.Types.Build where

import Data.Aeson
import Data.JSON.Schema hiding (content)
import Data.Text as T
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle hiding (Schema)


data BuildSpec = BuildSpec
    { projectName    :: T.Text      -- ^ Name of the project to run the build on
    , repoWorkingDir :: T.Text      -- ^ Working directory in the project to run the build
    , buildSteps     :: [BuildStep] -- ^ Build steps to run
    } deriving (Eq, Generic, Ord, Show, Typeable)


data BuildStep = BuildStep
    { stepName    :: T.Text -- ^ This steps name ..
    , description :: T.Text -- ^ A description, for humans
    , step        :: T.Text -- ^ Actual command to run e.g. "make"
    } deriving (Eq, Generic, Ord, Show, Typeable)

data BuildConfigCreationError =
    ProjectDoesNotExist String  -- ^ Specialized error
    | BuildConfigCreationError String -- ^ General error
    deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''BuildSpec "PFBuildSpec"
type instance PF BuildSpec = PFBuildSpec

instance FromJSON   BuildSpec
instance ToJSON     BuildSpec
instance JSONSchema BuildSpec where schema  = gSchema
instance XmlPickler BuildSpec where xpickle = gxpickle


deriveAll ''BuildStep "PFBuildStep"
type instance PF BuildStep = PFBuildStep

instance FromJSON   BuildStep
instance ToJSON     BuildStep
instance JSONSchema BuildStep where schema  = gSchema
instance XmlPickler BuildStep where xpickle = gxpickle


deriveAll ''BuildConfigCreationError "PFBuildConfigCreationError"
type instance PF BuildConfigCreationError = PFBuildConfigCreationError

instance FromJSON   BuildConfigCreationError
instance ToJSON     BuildConfigCreationError
instance JSONSchema BuildConfigCreationError where schema  = gSchema
instance XmlPickler BuildConfigCreationError where xpickle = gxpickle

instance ToResponseCode BuildConfigCreationError where
    toResponseCode _ = 418


