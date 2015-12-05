{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Snap.Snaplet.Assets (initAssets, addAssetRoutes, AssetConfig(..), addLanguage) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                  (ByteString)
import qualified Data.Configurator                as C
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text, unpack)
import           Data.Text.Encoding
import           Heist
import           Heist.Interpreted
import           Network.URI                      (isURIReference)
import           Snap
import           Snap.Snaplet.Assets.API
import qualified Snap.Snaplet.Assets.Handlers.CSS as CSS
import qualified Snap.Snaplet.Assets.Handlers.JS  as JS
import           Snap.Snaplet.Assets.Types
import           Snap.Snaplet.Heist.Compiled
import           Snap.Util.FileServe
import           System.FilePath
import           Text.XmlHtml                     as X

defaultCompileHandlers :: M.Map Text Assets
defaultCompileHandlers = M.fromList
    [ ("javascript", JS.assetsJS)
    , ("css", CSS.assetsCSS)
    ]

addLanguage :: Text -> Text -> (ByteString -> IO ByteString) -> AssetConfig -> AssetConfig
addLanguage e t cmp ac = ac { assetHandlers = addLang as1 } where
    as1 = assetHandlers ac
    addLang = M.alter addcmp e
        where
            addcmp Nothing = error $ "No handler registered for language " <> unpack e
            addcmp (Just as') = Just $ as' { compiler = newCompiler as' }
            newCompiler as' x | x == t = Just cmp
                              | otherwise = compiler as' x

addAssetRoutes :: Snaplet AssetConfig -> Initializer b v ()
addAssetRoutes (view snapletValue -> as) = do
    forM_ defaultCompileHandlers $ \ h ->
        addRoutes [(urlPrefixBS as <> "/" <> encodeUtf8 (assetType h), server h as)]
    when (environment as == Production && isURIReference (urlPrefix as)) $
        addRoutes [(urlPrefixBS as, serveDirectory (outputDir as))]

initAssets :: Snaplet (Heist b) -> Maybe [(Text, Assets)] -> SnapletInit b AssetConfig
initAssets h mch = makeSnaplet "assets" "Asset compiler" Nothing $ do
    cfg <- getSnapletUserConfig
    fp <- getSnapletFilePath
    [vendorDirM, outputDirM, sourceDirM, urlPrefixM, environmentM] <-
        liftIO $ mapM (C.lookup cfg) ["vendorDir", "outputDir", "sourceDir", "urlPrefix", "environment"]
    let vendorDir = fromMaybe (fp </> "vendor") vendorDirM
        outputDir = fromMaybe (fp </> "compiled") outputDirM
        sourceDir = fromMaybe (fp </> "src") sourceDirM
        urlPrefix = fromMaybe "assets/compiled" urlPrefixM
        assetHandlers = defaultCompileHandlers <> M.fromList (fromMaybe [] mch)
        environment = case environmentM of
            Nothing -> Development
            Just "Development" -> Development
            Just "Production" -> Production
            _ -> Development
    let a = AssetConfig {..}
    addConfig h $ mempty & scLoadTimeSplices .~ ("compile" ## compilerSplice a)
    return a

compilerSplice :: AssetConfig -> HeistT IO IO [Node]
compilerSplice as = do
    x <- getParamNode
    ns <- runNodeList $ X.elementChildren x
    case X.getAttribute "type" x of
        Just t -> case M.lookup t defaultCompileHandlers of
            Just f -> splicer f as $ filterElems ns
            Nothing -> error $ "Unhandled asset type " ++ show t
        Nothing -> error $ "<compile> tag found without 'type' attribute"
    where filterElems = mapMaybe (\ x -> case x of e@(X.Element {}) -> Just e; _ -> Nothing)
