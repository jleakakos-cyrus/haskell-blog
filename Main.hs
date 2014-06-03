{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (msum, forM_)
import Happstack.Server 
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Acid
import Core

import Data.Acid (openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)

main :: IO ()
main = do
  acid <- openLocalState emptyPosts
  ps <- query' acid GetPosts
  simpleHTTP nullConf $ msum [dir "posts" $ ok $ toResponse $ displayPosts ps]

displayPosts :: [Post] -> H.Html
displayPosts ps = 
  H.html $ do
    H.head $ do
      H.title (H.toHtml "All Posts")
    H.body $ do
      H.p (H.toHtml "Here are the posts:")
      H.ul $ forM_ ps (H.li . H.toHtml . show)
