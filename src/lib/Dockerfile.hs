
module Dockerfile where

import System.FilePath


data Dockerfile = Dockerfile {
        baseImage       :: String,
        orderedSteps    :: [String]
    } deriving (Eq, Show)


class ConstructionStep c where
    blueprint :: c -> String

data RunStep = RunStep {
        cmdToRun :: String
    } deriving (Eq, Show)

instance ConstructionStep RunStep where
    blueprint x = "RUN " ++ cmdToRun x

runStep :: String -> RunStep
runStep s = RunStep { cmdToRun = s }


data AddStep = AddStep String String deriving (Eq, Show)

instance ConstructionStep AddStep where
    blueprint ( AddStep f p ) = "ADD " ++ f ++ " " ++ p

addStep :: String -> String -> AddStep
addStep f p = AddStep f p

buildDockerfile :: ConstructionStep c => String ->  [c] -> Dockerfile
buildDockerfile i cs = Dockerfile { baseImage = i, orderedSteps = buildSteps cs }
    
buildSteps :: ConstructionStep c => [c] -> [String]
buildSteps = map blueprint 


writeDockerfile :: FilePath -> Dockerfile -> IO ()
writeDockerfile fp df = writeFile fp $ buildLines df
    where 
        buildLines :: Dockerfile -> String
        buildLines ( Dockerfile bi os ) =   
            "FROM " ++ bi ++ "\n" ++ unlines os
        

