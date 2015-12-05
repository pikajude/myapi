{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Splices (siteSplices) where

import           Control.Monad.Trans
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Text.Lazy                  (fromStrict)
import           Database.Persist
import           Heist
import qualified Heist.Compiled                  as C
import           Network.HTTP.Types
import           Snap
import           Snap.Snaplet.Persistent
import           Text.Markdown                   (Markdown (Markdown))
import           Web.PathPieces
------------------------------------------------------------------------------
import           Application
import           Forms
import           HighlightedMarkdown
import           Models
import           PersistUtils
import           Text.Digestive.Heist.ErrorAware

withCache :: RuntimeSplice AppHandler a -> (RuntimeSplice AppHandler a -> C.Splice AppHandler) -> C.Splice AppHandler
withCache = flip (C.deferMap return)

loginSplices :: Splices (C.Splice AppHandler)
loginSplices = "loginForm" ## formSplice mempty mempty (spliceForm "login" loginForm)

postSplices :: Splices (RuntimeSplice AppHandler (Entity Entry) -> C.Splice AppHandler)
postSplices = mapV C.pureSplice $ do
    "postTitle" ## C.textSplice (entryTitle . entityVal)
    "postId" ## C.textSplice (toPathPiece . entityKey)
    "postSlug" ## C.textSplice (entrySlug . entityVal)
    "postContent" ## C.htmlNodeSplice (markdownToSplice . Markdown . fromStrict . entryContent . entityVal)
    "postContentRaw" ## C.textSplice (entryContent . entityVal)

singleSplices :: Splices (C.Splice AppHandler)
singleSplices = "singleEntry" ## withCache getSinglePost (C.withSplices C.runChildren postSplices)
    where
        getSinglePost = lift $ do
            slug <- maybe pass return =<< getParam "slug"
            withTop db $ run404 $ getBy (UniqueEntry $ decodeUtf8 slug)

newSplices :: Splices (C.Splice AppHandler)
newSplices = "newEntry" ## (C.withSplices C.runChildren (do
     "entryForm" ## formSplice mempty mempty . (spliceForm "entry" . entryForm =<<)
     "formAction" ## C.pureSplice (C.textSplice (const "/n"))
     )
    (return Nothing))

editSplices :: Splices (C.Splice AppHandler)
editSplices = "editEntry" ## withCache getKeyedPost (C.withSplices C.runChildren $ do
        postSplices
        "entryForm" ## formSplice mempty mempty . (spliceForm "entry" . entryForm . Just . entityVal =<<)
        "formAction" ## C.pureSplice (C.textSplice (("/e/" <>) . toPathPiece . entityKey)))
    where
        getKeyedPost = lift $ do
            key <- getPathPiece "key"
            fmap (Entity key) $ withTop db $ run404 $ get key

homepageSplices :: Splices (C.Splice AppHandler)
homepageSplices = "homePage" ## withCache getAllPosts (C.withSplices C.runChildren $ do
        "postList" ## (C.manyWithSplices C.runChildren postSplices)
        "disqusUrl" ## C.pureSplice (\ es ->
            let urls = map (\(Entity _ post) ->
                    ("2", Just $ T.pack "https://jude.bio/r/" <> entrySlug post)) es
                qs = renderQueryText True urls
             in "//otters.disqus.com/count-data.js" <> qs
                       )
        )
    where getAllPosts = lift $ withTop db $ runPersist (selectList [] [Desc EntryCreatedAt])

siteSplices :: Splices (C.Splice AppHandler)
siteSplices = mconcat [homepageSplices, singleSplices, editSplices, newSplices, loginSplices]
