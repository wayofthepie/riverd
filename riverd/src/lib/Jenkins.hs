{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Jenkins where

import Control.Lens hiding (deep)           -- lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson.Lens                      -- lens-aeson
import Data.Map (empty)
import Data.Text hiding (empty)
import Jenkins.Rest (Jenkins, (-?-), (-=-)) -- libjenkins
import qualified Jenkins.Rest as JR
import Text.Hamlet.XML
import Text.XML

import Prelude hiding (unwords)

{-
data BuildPlan = BuildPlan {
        vcsInfo     :: Maybe VCSRoot,
        buildWith   :: Text,
        extraParams :: [Text]
    } deriving(Eq, Show)


-- | Information about a VCS root.
class VCSInfo v where
    vcsTool     :: v -> Text
    vcsRootUrl  :: v -> Text

data VCSRoot = VCSGit VCSInfoGit | VCSSvn VCSInfoSvn deriving (Eq, Show)

data VCSInfoGit = VCSInfoGit {
        gitRepoUrl :: Text
    } deriving (Eq, Show)

instance VCSInfo VCSInfoGit where
    vcsTool _  = "git"
    vcsRootUrl = gitRepoUrl

data VCSInfoSvn = VCSInfoSvn {
        svnRepoUrl :: Text
    } deriving (Eq, Show)

instance VCSInfo VCSInfoSvn where
    vcsTool _  = "svn"
    vcsRootUrl = svnRepoUrl




-- | The jenkins master (hardcoded for now)
master :: JR.Master
master = JR.defaultMaster &
    JR.url .~ "http://192.168.59.103:8080"


-- | Test configration, describes a job with a single build step that
-- echoes test
testConfig :: Element
testConfig = Element "project" empty [xml|
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


testPlan = BuildPlan {
    vcsInfo = Just $
        VCSGit $
            VCSInfoGit "https://github.com/wayofthepie/github-maven-example",
    buildWith = "mvn",
    extraParams = ["clean", "install"]
}


gitPluginVersion = "git@2.3.1"

-- |
-- To be accurate about plugin values, the api should be queried at
-- http://(jenkins)/pluginManager/api/json?depth=1.
--
-- TODO: Create a converter for plans to xml.
plan2Cfg :: BuildPlan -> Element
plan2Cfg b = Element "project" empty [xml|
    <actions>
    <description>
    <keepDependencies>
        false
    <properties>
    <scm class=hudson.plugins.git.GitSCM>
        <configVersion>
            2
        <userRemoteConfigs>
            <hudson.plugins.git.UserRemoteConfig>
                <url>#{repoUrl b}
        <branches>
            <hudson.plugins.git.BranchSpec>
                <name>
                    */master
        <doGenerateSubmoduleConfigurations>
            false
        <submoduleCfg class="list">
        <extensions>
    <canRoam>
        true
    <disabled>
        false
    <blockBuildWhenDownstreamBuilding>
        false
    <blockBuildWhenUpstreamBuilding>
        false
    <triggers>
    <concurrentBuild>
        false
    <builders>
        <hudson.tasks.Shell>
            <command>#{buildWith b} #{unwords $ extraParams b}
    <publishers>
    <buildWrappers>
|]


-- | Create a job named __n__ with the config __c__
createJob :: ( MonadBaseControl IO m, MonadIO m ) => Text -> Element -> m ( JR.Result () )
createJob n c =
    JR.run (JR.defaultMaster &
        JR.url .~ ("http://192.168.59.103:8080/")) $
            JR.postXml ("createItem" -?- "name" -=- n) $ e2bs c
    where
        e2bs xml = renderLBS def $ Document (Prologue [] Nothing []) xml  []

-}
