{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

-- Custom application data
import Acid
import Core

-- Handle server start more safely
import Control.Exception (bracket)

-- Web server and routing
import Control.Monad (msum, forM_)
import Happstack.Server

-- Templating
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Storage
import Data.Acid (AcidState, openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)

-- Logging
import System.Log.Logger ( updateGlobalLogger
                         , rootLoggerName
                         , setLevel
                         , Priority(..)
                         )

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  bracket (openLocalState emptyPosts)
           createCheckpointAndClose
           (\acid -> simpleHTTP nullConf (handlers acid))

handlers :: AcidState Posts -> ServerPart Response
handlers acid = msum
  [
    dirs "posts/new" $ do
      method GET
      ok $ toResponse displayNewPostForm,
    dirs "posts/new" $ do
      method POST
      post <- updatePosts acid
      ok $ toResponse (show post),
    dirs "posts" $ do
      ps <- retrievePosts acid
      ok $ toResponse (displayPosts ps)
  ]

updatePosts :: AcidState Posts -> ServerPartT IO Post
updatePosts acid = do
  decodeBody myPolicy
  title <- body $ look "title"
  contents <- body $ look "content"
  post <- update' acid (AddPost title contents)
  return post

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 10240 10240)

retrievePosts :: AcidState Posts -> ServerPartT IO [Post]
retrievePosts acid = do
  ps <- query' acid GetPosts
  return ps

displayPosts :: [Post] -> H.Html
displayPosts ps =
  H.html $ do
    H.head $ do
      H.title "All Posts"
    H.body $ do
      H.p "Here are the posts:"
      H.ul $ forM_ ps (H.li . H.toHtml . show)

displayNewPostForm :: H.Html
displayNewPostForm =
  H.html $ do
    H.head $ do
      H.title "New Post"
    H.body $ do
      H.form ! A.action "/posts/new" ! A.method "post" $ do
        H.div $ do
          H.label ! A.for "title" $ "Title"
          H.input ! A.type_ "text" ! A.id "title" ! A.name "title"
        H.div $ do
          H.label ! A.for "content" $ "Content"
          H.input ! A.type_ "text" ! A.id "content" ! A.name "content"
        H.button ! A.type_ "submit" $ "Submit Post"
