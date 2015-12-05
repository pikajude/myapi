{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Snap.Snaplet.Assets.API (
    -- * Datatypes
    Assets (..),
    AssetSource (..),
    AssetFile (..),
    AssetInline (..),

    server, splicer
) where

import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 hiding (concat, map, reverse)
import           Data.Text.Encoding
import           Heist
import           Snap
import           Snap.Snaplet.Assets.Types
import           Snap.Snaplet.Assets.Utils
import           System.Directory
import           System.FilePath
import           Text.XmlHtml
import           Web.PathPieces

splicer :: MonadIO m => Assets -> AssetConfig -> [Node] -> HeistT m m [Node]
splicer x as ns = do
    let sources = mapMaybe fromNode ns

    if environment as == Development
        then return $ map (toNode x . condense) sources
        else do
            filename <- liftIO $ genFileName x as sources
            let fp = outputDir as </> filename
            ensureFileWithContents fp (do
                putStrLn $ "Compiling assets " ++ show sources ++ " to destination " ++ filename
                allContents <- B.concat <$> mapM (compile x as) sources
                minifier x allContents
                )
            return [toNode x (Left (urlPrefix as </> filename))]

    where
        condense (Inline AssetInline{..}) = Right aiSource
        condense (File f) = Left $ unpack $ intercalate "/" $ pack (urlPrefix as) : assetType x : toPathMultiPiece f

{-
    bs <- liftIO $ B.concat <$> mapM (compile as) locals
    return [Element "script" [] [TextNode $ decodeUtf8 bs]]
    -}

genFileName :: MonadIO m => Assets -> AssetConfig -> [AssetSource] -> m String
genFileName x as sources = do
    fingerprint <- unpack . decodeUtf8 . hashScript . mconcat <$> mapM hashBytes sources
    return $ "compiled-" <> fingerprint <.> fileExtension x
    where
        hashBytes (Inline (AssetInline t s)) = return $ encodeUtf8 (toPathPiece t <> s)
        hashBytes (File f@(AssetFile t v p)) =
            (\ mt -> mt <> encodeUtf8 (toPathPiece t
                                   <> (if v then "vendor" else mempty)
                                   <> pack p))
            <$> getMtime f
        hashScript s = digestToHexByteString (hash s :: Digest SHA1)
        getMtime f = do
            t <- liftIO $ getModificationTime (getSource as f)
            return . encodeUtf8 . pack $ show t

getSource :: AssetConfig -> AssetFile -> FilePath
getSource as AssetFile{..}
    | afVendor = vendorDir as </> afPath
    | otherwise = sourceDir as </> afPath

getLanguage :: AssetSource -> Text
getLanguage (File AssetFile{..}) = afLang
getLanguage (Inline AssetInline{..}) = aiLang

-- | The vanilla, uncompiled contents of the asset. The default
-- definition of this method uses 'HasSourceFile', but for an inline
-- Assets, you should override it to return the contents of the HTML
-- node.
readContents :: AssetConfig -> AssetSource -> IO ByteString
readContents as (File a) = B.readFile (getSource as a)
readContents _ (Inline a) = return $ encodeUtf8 (aiSource a)

{-
-- | A way to determine whether a certain Assets language requires
-- compilation before it can be served to the user.
class Compiler t where
    -- | @'Just' f@ if there exists a compiler function for the given
    -- language; 'Nothing' if AssetConfig of this language do not need
    -- compilation.
    compiler :: t -> Maybe (ByteString -> IO ByteString)

-- | Serve a single script, performing any necessary compilation; this
-- method shortcuts to 'sendFile' if possible. Note that you don't need to
-- (and indeed can't) implement this class for your inline AssetConfig, since
-- serving them from a separate URI wouldn't make sense.
--
-- This function should only be needed in development mode; in production
-- mode, you should use 'compile' to concatenate your scripts, minify them
-- and store them.
serveScript :: (MonadSnap m, Assets a, HasSourceFile (AssetLocal a), Compiler (AssetLanguage a)) => AssetConfig -> AssetLocal a -> m ()
serveScript as s = case compiler (getLanguage s) of
    Just c -> liftIO (readContents as s >>= c) >>= writeBS
    Nothing -> sendFile (getSource as s)

-- | A handler that serves AssetConfig determined from the current filepath.
serve :: (PathMultiPiece (AssetLocal a), HasSourceFile (AssetLocal a), MonadSnap m, Assets a, Compiler (AssetLanguage a)) => Proxy (AssetLocal a) -> AssetConfig -> m ()
serve p as = do
    r <- decodeUtf8 . rqPathInfo <$> getRequest
    case fromPathMultiPiece (split (=='/') r) of
        Nothing -> error "uh-oh"
        Just x -> serveScript as (x `asProxyTypeOf` p)

-- | Utility function that compiles (if needed) and returns the compiled
-- contents of a given asset. In production mode, this function could be
-- used as follows:
--
-- @
--     allScripts \<- concat \<$\> mapM compile myScripts
--     minified <- runMyMinifier allScripts
--     return minified
-- @
compile :: (Assets a, Compiler (AssetLanguage a)) => AssetConfig -> AssetLocal a -> IO ByteString
compile as s = readContents as s >>= fromMaybe return (compiler (getLanguage s))
-}

compile :: Assets -> AssetConfig -> AssetSource -> IO ByteString
compile x as s = do
    cnt <- readContents as s
    fromMaybe return (compiler x (getLanguage s)) cnt

server :: MonadSnap m => Assets -> AssetConfig -> m ()
server p as = do
    r <- decodeUtf8 . rqPathInfo <$> getRequest
    case fromPathMultiPiece (split (== '/') r) of
        Nothing -> pass
        Just y@(File -> x) -> do
            modifyResponse $ setContentType (contentType p)
            case compiler p (getLanguage x) of
                Just c -> liftIO (readContents as x >>= c) >>= writeBS
                Nothing -> sendFile (getSource as y)
