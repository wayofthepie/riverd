{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Jenkins where

import Control.Lens hiding (deep)           -- lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Aeson.Lens                      -- lens-aeson
import Data.ByteString.Lazy.Internal
import Data.Map (empty)
import Data.Text hiding (empty)
import Jenkins.Rest (Jenkins, (-?-), (-=-)) -- libjenkins
import qualified Jenkins.Rest as JR
import Text.Hamlet.XML
import Text.XML.HXT.Core 
import Text.XML

-- Hardcode the master for now
master :: JR.Master
master = JR.defaultMaster &
    JR.url .~ "http://192.168.59.103:8080"

config :: Element
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

createJob :: ( MonadBaseControl IO m, MonadIO m ) => Text -> m ( JR.Result () )
createJob n = 
    JR.run (JR.defaultMaster &
        JR.url .~ ("http://192.168.59.103:8080/")) $
            JR.postXml ("createItem" -?- "name" -=- n) configData
    where
        configData = renderLBS def $ Document (Prologue [] Nothing []) config []


