{-# LANGUAGE NullaryTypeClasses #-}

module GitAnalyzer where

import Data.Maybe
import System.Directory
import System.FilePath
import System.Process

data BuildSystem = BuildSystem {
        buildSystem :: String
    } deriving (Eq, Show)

data SimpleMavenPlan = SimpleMavenPlan {
        pathToPom   :: FilePath,
        goals       :: [String]
    } deriving (Eq, Show)


getBuildSystem :: FilePath -> IO ( Maybe BuildSystem )
getBuildSystem f = doesDirectoryExist f >>=
    \x -> case x of
        True    -> analyze f
        False   -> error "Not a directory ..."

analyze :: FilePath -> IO ( Maybe BuildSystem )
analyze f = getDirectoryContents f >>=
      \lf -> if elem "build.gradle" lf then
                return $ Just $ gen "gradle"
             else if elem "pom.xml" lf then
                return $ Just $ gen "maven"
             else
                return Nothing
    where gen s = BuildSystem { buildSystem = s }



-- | Convert an execution plan to a runnable process
-- Just deal with SimpleMavenPlan's for now
planToProcess :: SimpleMavenPlan -> CreateProcess
planToProcess ( SimpleMavenPlan p gs ) =
    (proc "mvn" gs){ cwd = Just p }


--createProcess (proc "mvn" ["clean", "install"]){ cwd = Just "/opt/repos/github-maven-example/example/"} >>= \(_,_,_,h) -> waitForProcess h
