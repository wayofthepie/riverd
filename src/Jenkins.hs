{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenkins where

import Control.Lens hiding (deep)           -- lens
import Control.Monad
import Data.Aeson.Lens                      -- lens-aeson
import Data.ByteString.Lazy.Internal
import Jenkins.Rest (Jenkins, (-?-), (-=-)) -- libjenkins
import qualified Jenkins.Rest as JR
import Text.XML.HXT.Core 

-- Hardcode the master for now
master :: JR.Master
master = JR.defaultMaster &
    JR.url .~ "http://192.168.59.103:8080"

data BuildConfig = BuildConfig {
        actions         :: String,
        description     :: String,
        keepDeps        :: String,
        properties      :: String,
        scm             :: String,
        canRoam         :: String,
        disabled        :: String,
        blockFromDown   :: String,
        blockFromUp     :: String,
        triggers        :: String,
        concurrentBuild :: String,
        buildSteps      :: String,
        publishers      :: String,
        buildWrappers   :: String
    } deriving (Eq, Show)

{-    actions         :: Maybe [String],
        description     :: Maybe String,
        keepDeps        :: Bool,
        properties      :: Maybe [String],
        scm             :: SCM,
        canRoam         :: Bool,
        disabled        :: Bool,
        blockFromDown   :: Bool,
        blockFromUp     :: Bool,
        triggers        :: Maybe [String],
        concurrentBuild :: Bool,
        buildSteps      :: Maybe [BuildStep],
        publishers      :: Maybe [String],
        buildWrappers   :: Maybe [String]
    } deriving (Eq, Show)-}


data SCM = SCM {
        scmClass        :: String
    } deriving (Eq, Show)
   

data BuildStep = BuildStep {
        task    :: Maybe String
    } deriving (Eq, Show)


atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text


getBuildConfig = atTag "project" >>>
    proc x -> do 
        actions         <- textAtTag "actions"          -< x
        description     <- textAtTag "description"      -< x
        keepDeps        <- textAtTag "keepDependencies" -< x
        properties      <- textAtTag "properties"       -< x
        scm             <- textAtTag "scm"              -< x
        canRoam         <- textAtTag "canRoam"          -< x
        disabled        <- textAtTag "disabled"         -< x
        blockFromDown   <- textAtTag "blockBuildWhenDownstreamBuilding" -< x
        blockFromUp     <- textAtTag "blockBuildWhenUpstreamBuilding"   -< x
        triggers        <- textAtTag "triggers"         -< x
        concurrentBuild <- textAtTag "concurrentBuild"  -< x
        builders        <- textAtTag "builders"         -< x
        publishers      <- textAtTag "publishers"       -< x
        buildWrappers   <- textAtTag "buildWrappers"    -< x
        returnA -< (actions, description)

parseXml doc = readString [ withValidate no, withRemoveWS yes ] doc

retrieveConfig = 
    JR.run (JR.defaultMaster & 
        JR.url .~ "http://192.168.59.103:8080/job/test/config.xml") $ 
            JR.get JR.xml ("/")


bcResult :: IO ()
bcResult = do 
    doc     <- xtr retrieveConfig
    xml     <- return $ parseXml doc
    result  <- runX ( xml >>> getBuildConfig )
    case result of
        []  -> putStrLn "Error parsing..."
        w:_ -> print w
