{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Jenkins where

import Control.Lens hiding (deep)           -- lens
import Control.Monad
import Data.Aeson.Lens                      -- lens-aeson
import Data.ByteString.Lazy.Internal
import Data.Map (empty)
import Jenkins.Rest (Jenkins, (-?-), (-=-)) -- libjenkins
import qualified Jenkins.Rest as JR
import Text.Hamlet.XML
import Text.XML.HXT.Core 
import Text.XML

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

{-
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
    doc     <- retrieveConfig
    xml     <- return $ parseXml doc
    result  <- runX ( xml >>> getBuildConfig )
    case result of
        []  -> putStrLn "Error parsing..."
        w:_ -> print w-}


config = Element "project" empty [xml|
    <actions>
    <description>
    <keepDependencies>
        false
    <properties>
    <scm class="hudson.scm.NullSCM">
    <canRoam>true
    <disabled> false
    <blockBuildWhenDownstreamBuilding>false
    <blockBuildWhenUpstreamBuilding>false
    <triggers>
    <concurrentBuild>
        false
    <builders>
        <hudson.tasks.Shell>
            <command>
                echo "test"
    <publishers>
    <buildWrappers>
|]


createJob n = 
    JR.run (JR.defaultMaster &
        JR.url .~ ("http://192.168.59.103:8080/")) $
            JR.postXml ("createItem" -?- "name" -=- n) configData
    where
        configData = renderLBS def $ Document (Prologue [] Nothing []) config []


