
-- | Convenience functions to view xml structure of types,
-- these should be moved to test....
module Model.XmlGen where


import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.Arrow.Pickle
import Control.Arrow
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.XmlState

import Model.Types


genXmlFile :: a -> PU a -> String -> IO [XmlTree]
genXmlFile togen pickler fileName =
    runX ( constA togen >>> xpickleDocument pickler [withIndent yes] fileName)


exampleProject =
    Project "test"
        [ExternalDependency "gradle" "2.2.0" "Gradle!"]
        ["gradle clean build"]


testGenFromProject :: IO [XmlTree]
testGenFromProject =
    genXmlFile exampleProject (xpickle::PU Project) "test.xml"
