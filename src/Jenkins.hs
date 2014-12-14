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


-- | Create a job named __n__ with the config __c__
createJob :: ( MonadBaseControl IO m, MonadIO m ) => Text -> Element -> m ( JR.Result () )
createJob n c = 
    JR.run (JR.defaultMaster &
        JR.url .~ ("http://192.168.59.103:8080/")) $
            JR.postXml ("createItem" -?- "name" -=- n) $ e2bs c
    where 
        e2bs xml = renderLBS def $ Document (Prologue [] Nothing []) xml  []


