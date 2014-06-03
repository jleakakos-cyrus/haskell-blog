{-# LANGUAGE TemplateHaskell,
             DeriveDataTypeable,
             RecordWildCards,
             TypeFamilies #-}
module Acid where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Core

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

$(makeAcidic ''Posts ['addPost, 'getPosts])
