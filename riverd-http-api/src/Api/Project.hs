{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api.Project (resource) where

import Control.Monad.Reader
import Data.Aeson
import Data.JSON.Schema hiding (content)
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Generic.Aeson
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest
import qualified Rest.Resource as R
import Text.XML.HXT.Arrow.Pickle

type Title = String

data Post = Post
    { title :: Title
    , content :: String
    } deriving (Generic, Typeable, Show)

deriveAll ''Post "PFPost"

type instance PF Post = PFPost
instance FromJSON   Post where parseJSON = gparseJson
instance ToJSON     Post where toJSON    = gtoJson
instance JSONSchema Post where schema    = gSchema
instance XmlPickler Post where xpickle   = gxpickle

resource :: Resource IO (ReaderT Title IO) Title () Void
resource = mkResourceReader
    { R.name    = "post"
    , R.schema  = withListing () $ named [("title", singleBy id)]
    , R.list    = const list
    , R.get     = Just get
    }

get :: Handler (ReaderT Title IO)
get = mkIdHandler xmlJsonO $ \_ title -> liftIO $ readPost title

readPost :: Title -> IO Post
readPost t = return $ Post { title = t, content = "Hey!" }

list :: ListHandler IO
list = mkListing xmlJsonO $ \range -> lift $ readPosts (offset range) (count range)

readPosts :: Int -> Int -> IO [Post]
readPosts _ _ = return
    [
        Post { title = "Post One", content = "First." },
        Post { title = "Post Two", content = "Second!" }
    ]


