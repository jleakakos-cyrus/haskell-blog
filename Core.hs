{-# LANGUAGE DeriveDataTypeable #-}

module Core where

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

emptyPosts :: Posts
emptyPosts = Posts { posts = [] }
