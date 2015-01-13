{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Aeson
import Data.JSON.Schema
import Data.JSON.Schema.Validate as JSV
import Data.Monoid
import Data.Proxy()
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs
import Model.Repository
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.XPath.XPathEval

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
        ],
        testGroup "Xml TypeValidation" [
            testGroup "Validate Project To Xml" [
                testCase "Validate Title" test_validate_Project_to_Xml_Title
            ]
        ]
    ]


-- | Project type conversion tests.

-- | Validates whether a Project's JSON Schema is valid by converting a Project
--  to JSON using aeson's toJSON, generating a JSON Schema with schema and
--  validating the Schema works for the generated Project JSON.
test_validate_Project_to_Json = (JSV.isValid (schema (Proxy :: Proxy Project)) $
        (toJSON :: Project -> Value) exampleProject) @?= True

pickledProject = (pickleDoc :: PU Project -> Project -> XmlTree) xpickle exampleProject


test_validate_Project_to_Xml_Title = getValue @?= ( Just $ projectTitle exampleProject )
    where
        mapXtractTxt    = fmap (\(XText t) -> t)
        mapGetNode      = fmap getNode
        xpath           = getXPath "//title/text()" pickledProject
        validate xs     | length xs == 1 = Just $ head xs
                        | otherwise = Nothing
        getValue = validate $ mapXtractTxt $ mapGetNode $ xpath
