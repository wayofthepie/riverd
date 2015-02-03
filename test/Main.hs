{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Aeson
import Data.JSON.Schema
import Data.JSON.Schema.Validate as JSV
import Data.Monoid
import Data.Proxy()
import qualified Data.Text as T
import Data.Tree.Class
import Data.Tree.NTree.TypeDefs
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

import Api.Types.Project

emptyTestOpts = mempty :: TestOptions

testOpts = emptyTestOpts {
        topt_maximum_generated_tests = Just 500000,
        topt_maximum_unsuitable_generated_tests = Just 20000000
    }

emptyRunnerOpts = mempty :: RunnerOptions

runnerOpts = emptyRunnerOpts { ropt_test_options = Just testOpts }


main :: IO ()
main = defaultMainWithOpts tests runnerOpts



-------------------------------------------------------------------------------
-- | The list of tests this test module runs.

tests = [
        testGroup "Json Type Validation" [
            testCase "Validate Project To Json" test_validate_Project_to_Json
        ],        testGroup "Xml TypeValidation" [
            testGroup "Validate Project To Xml" [
                testCase "Validate Name" test_validate_Project_to_Xml
            ]
        ]
    ]

-------------------------------------------------------------------------------
-- | Project type conversion tests.

exampleProject :: Project
exampleProject = Project "test" "url"


-- | Validates whether a Project's JSON Schema is valid by converting a Project
--  to JSON using aeson's toJSON, generating a JSON Schema with schema and
--  validating the Schema works for the generated Project JSON.
test_validate_Project_to_Json = (JSV.isValid (schema (Proxy :: Proxy Project)) $
        (toJSON :: Project -> Value) exampleProject) @?= True


pickledProject =
    (pickleDoc :: PU Project -> Project -> XmlTree) xpickle exampleProject


-- | Validate whether a project can successfully be converted into xml.
test_validate_Project_to_Xml = do
    getNameValue @?= ( Just $ T.unpack $ name exampleProject )
    getUrlValue  @?= ( Just $ T.unpack $ repoUrl exampleProject )
    where
        mapXtractTxt    = fmap (\(XText t) -> t)
        mapGetNode      = fmap getNode
        xpathName       = getXPath "//project/name/text()" pickledProject
        xpathUrl        = getXPath "//project/repoUrl/text()" pickledProject
        validate xs     | length xs == 1 = Just $ head xs
                        | otherwise = Nothing
        validateXMap xp = validate $ mapXtractTxt $ mapGetNode $ xp
        getNameValue    = validateXMap xpathName
        getUrlValue     = validateXMap xpathUrl




