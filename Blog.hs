{-# LANGUAGE TemplateHaskell,
             DeriveDataTypeable,
             RecordWildCards,
             TypeFamilies #-}
module Blog where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)

type PostId = Int
type Title = String
type Content = String

data Post = Post { title :: Title, content :: Content, postId :: PostId }
  deriving (Eq, Read, Show, Data, Typeable)

instance Ord Post where
  compare a b = compare (postId a) (postId b)

data Posts = Posts { posts :: [Post] }
 deriving (Eq, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Post)
$(deriveSafeCopy 0 'base ''Posts)

-- addPost :: Title -> Content -> State Posts Post
addPost :: Title -> Content -> Update Posts Post
addPost title content = do
  Posts ps <- get
  let nextPostCount = (length ps) + 1
  let newPost = Post title content nextPostCount
  put $ Posts (newPost:ps)
  return newPost

-- getPosts :: Reader Posts [Post]
getPosts :: Query Posts [Post]
getPosts = posts <$> ask

emptyPosts :: Posts
emptyPosts = Posts { posts = [] }

$(makeAcidic ''Posts ['addPost, 'getPosts])
