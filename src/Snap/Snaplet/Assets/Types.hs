{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.Assets.Types where

import Data.ByteString    (ByteString)
import Data.Map           (Map)
import Data.Text          (Text, intercalate, pack, unpack)
import Data.Text.Encoding
import Text.XmlHtml
import Web.PathPieces

-- | Convert 'Assets' into a handler that can read, compile, and serve
-- assets.
instance PathMultiPiece AssetFile where
    toPathMultiPiece (AssetFile l v p) = toPathPiece l : (if v then ("vendor" :) else id) [toPathPiece p]
    fromPathMultiPiece (t : "vendor" : ts) = Just $ AssetFile t True (unpack $ intercalate "/" ts)
    fromPathMultiPiece (t : ts) = Just $ AssetFile t False (unpack $ intercalate "/" ts)
    fromPathMultiPiece _ = Nothing

-- | Represents all assets found in the HTML source.
data AssetSource = Inline AssetInline -- ^ Inline javascript, css, etc.
                 | File AssetFile -- ^ Asset sourced from somewhere on the filesystem.
                   deriving Show

-- | An asset somewhere on the filesystem.
data AssetFile = AssetFile
               { afLang   :: Text
               , afVendor :: Bool
               , afPath   :: FilePath
               } deriving Show

-- | An asset embedded in the document.
data AssetInline = AssetInline
                 { aiLang   :: Text
                 , aiSource :: Text
                 } deriving Show

-- | Everything needed to compile, parse, and serve a list of assets.
data Assets = Assets
            { assetType     :: Text -- ^ @\<compile type="$assetType"\>@
            , fileExtension :: String
            , contentType   :: ByteString -- ^ MIME type for this content
            , toNode        :: Either FilePath Text -> Node
            -- ^ Used in development mode only.
            --
            -- Convert an asset to an HTML node for inclusion in the
            -- page. The first argument converts an 'AssetFile' to a path
            -- ('Text' for use in an 'Element'). The resulting path is
            -- routed to the 'AssetFile' in development mode.
            --
            -- > myToNode mkUrl (File assetFile) = Element "script" [("src", mkUrl f)] []
            , compiler      :: Text -> Maybe (ByteString -> IO ByteString)
            -- ^ Return an IO action that can compile an asset of the
            -- given type, or 'Nothing' if none is needed.
            , minifier      :: ByteString -> IO ByteString
            }

data Environment = Development | Production deriving (Eq, Show)

data AssetConfig = AssetConfig
            { vendorDir     :: FilePath
            , outputDir     :: FilePath
            , sourceDir     :: FilePath
            , assetHandlers :: Map Text Assets
            , urlPrefix     :: String
            , environment   :: Environment
            }

lookupTy :: Text -> [Assets] -> Maybe Assets
lookupTy t (t1 : ts) | t == assetType t1 = Just t1
                     | otherwise = lookupTy t ts
lookupTy _ [] = Nothing

urlPrefixBS :: AssetConfig -> ByteString
urlPrefixBS = encodeUtf8 . pack . urlPrefix

fromNode :: Node -> Maybe AssetSource
fromNode e@(Element lang ats _) = Just $ case lookup "src" ats of
    Just src -> File (AssetFile lang (any ((== "vendor") . fst) ats) (unpack src))
    Nothing -> Inline (AssetInline lang (nodeText e))
fromNode _ = Nothing
