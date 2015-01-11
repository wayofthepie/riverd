module Main where


import Data.Aeson
import Data.JSON.Schema
import Data.JSON.Schema.Validate as JSV
import Data.Monoid
import Data.Proxy()
import Model.Repository
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

emptyTestOpts = mempty :: TestOptions

testOpts = emptyTestOpts {
        topt_maximum_generated_tests = Just 500000,
        topt_maximum_unsuitable_generated_tests = Just 20000000
    }

emptyRunnerOpts = mempty :: RunnerOptions

runnerOpts = emptyRunnerOpts { ropt_test_options = Just testOpts }


exampleProject :: Project
exampleProject = Project "Test" ["t1", "t2"]


main :: IO ()
main = defaultMainWithOpts tests runnerOpts


tests = [
        testGroup "Json Type Validation" [
            testCase "Validate Project To Json" test_validate_Project_to_Json
        ]
    ]

test_validate_Project_to_Json = (JSV.isValid (schema (Proxy :: Proxy Project)) $
        (toJSON :: Project -> Value) exampleProject) @?= True

