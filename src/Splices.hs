{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Splices (siteSplices, compileSplice) where

import           Control.Monad
import           Control.Monad.Trans
import           Crypto.Hash
import qualified Data.ByteString.Lazy            as B
import           Data.List
import           Data.Maybe
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
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ByteString.Lazy  (readProcessWithExitCode)
import           Text.Markdown                   (Markdown (Markdown))
import           Text.Printf
import           Text.XmlHtml
import           Web.PathPieces
------------------------------------------------------------------------------
import           Application
import           Forms
import           HighlightedMarkdown
import           Models
import           PersistUtils
import           RunHandler
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

compileSplice :: String -> C.Splice AppHandler
compileSplice "devel" = C.runChildren
compileSplice "prod" = compileScriptsSplice
compileSplice x = error $ "Unknown environment " ++ x

compileScriptsSplice :: C.Splice AppHandler
compileScriptsSplice = do
    n <- getParamNode
    let children = childNodes n
        srcs = catMaybes $ map (getAttribute "src") children
        outputFilename = show (hash (encodeUtf8 $ T.intercalate ":" $ sort srcs) :: Digest SHA1) ++ ".js"
    return $ C.yieldRuntime $ do
        e <- liftIO $ doesFileExist $ outputDir </> outputFilename
        unless e $ do
            fileSources <- lift $ mapM (runSelf . encodeUtf8) srcs
            liftIO $ do
                ugly <- uglify srcs (B.concat fileSources)
                createDirectoryIfMissing True outputDir
                B.writeFile (outputDir </> outputFilename) ugly
        return $ C.htmlNodeSplice (const [Element "script" [("src", T.pack $ "/s" </> "compiled" </> outputFilename)] []]) ()
    where
        outputDir = "static/compiled"

uglify :: [T.Text] -> B.ByteString -> IO B.ByteString
uglify paths code = do
    (status, out, err) <- readProcessWithExitCode "uglifyjs" ["-c", "-m"] code
    case status of
        ExitSuccess -> return out
        ExitFailure i -> error $ printf "Error minifying %s (shell exited %d): %s"
            (show paths) i (T.unpack $ decodeUtf8 $ B.toStrict err)
