{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module AssetCompilation where

import           Control.Lens              ((&), (.~))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Crypto.Hash
import           Data.Byteable
import qualified Data.ByteString           as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy      as B (toStrict)
import qualified Data.Configurator         as C
import           Data.Configurator.Types   (Configured (..))
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Heist
import           Heist.Interpreted
import           Paths_myapi
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ByteString (readProcessWithExitCode)
import           Text.Printf
import qualified Text.XmlHtml              as X

data AssetType = Remote | Compiled Text | Static | Vendored | Inline
                 deriving Show

data Script = Script
            { scriptType   :: AssetType
            , scriptSource :: Text
            } deriving Show

data Stylesheet = Stylesheet
                { stylesheetType   :: AssetType
                , stylesheetSource :: Text
                } deriving Show

data Asset = ScriptAsset Script | StylesheetAsset Stylesheet

data Assets = Assets
            { topDir        :: FilePath
            , vendorDir     :: FilePath
            , coffeeSrcDir  :: FilePath
            , coffeeDestDir :: FilePath
            , outputDir     :: FilePath
            } deriving (Show)

initCompiler h = makeSnaplet "assets" "Asset compiler" Nothing $ do
    cfg <- getSnapletUserConfig
    fp <- getSnapletFilePath
    [vendorDir, coffeeSrcDir, coffeeDestDir, outputDir] <- liftIO $
        mapM (C.lookup cfg) ["vendorDir", "coffeeSrcDir", "coffeeDestDir", "outputDir"]
    let assets = Assets
               { topDir = fp
               , vendorDir = fromMaybe (fp </> "vendor") vendorDir
               , coffeeSrcDir = fromMaybe (fp </> "coffee") coffeeSrcDir
               , coffeeDestDir = fromMaybe (fp </> "js") coffeeDestDir
               , outputDir = fromMaybe (fp </> "compiled") outputDir
               }
    addCompilerSplices h assets
    return assets

addCompilerSplices h a = do
    liftIO $ print a
    addConfig h $ mempty & scLoadTimeSplices .~ "compile" ## compileSplice a

-- compileSplice :: Assets -> HeistT IO IO [X.Node]
scriptSplice as = do
    x <- getParamNode
    compiles <- runNodeList $ X.childNodes x
    generateCompiledScripts as (mapMaybe extractScript compiles)

cssSplice as = do
    x <- getParamNode
    compiles <- runNodeList $ X.childNodes x
    generateCompiledStylesheets as (mapMaybe extractStyleSheet compiles)

scriptKey Script {..} = encodeUtf8 $ T.pack (show scriptType) <> scriptSource

sheetKey Stylesheet {..} = encodeUtf8 $ T.pack (show stylesheetType) <> stylesheetSource

extractAsset e@(X.Element "js" _ _) = fmap ScriptAsset $ extractScript e
extractAsset e@(X.Element "css" _ _) = fmap StylesheetAsset $ extractStyleSheet e
extractAsset e@(X.Element x _ _) = error $ "Unknown asset type " ++ T.unpack x
extractAsset _ = Nothing

extractScript e@(X.Element t _ _) = case t of
    "inline" -> Just $ Script Inline $ X.nodeText e
    "coffee" -> Script Compiled <$> X.getAttribute "src" e
    "vendor" -> Script Vendored <$> X.getAttribute "src" e
    "remote" -> Script Remote <$> X.getAttribute "src" e
    _ -> Nothing
extractScript _ = Nothing

extractStyleSheet e@(X.Element t _ _) = case t of
    "inline" -> Just $ Stylesheet Inline $ X.nodeText e
    "sass" -> Stylesheet Compiled <$> X.getAttribute "src" e
    "static" -> Stylesheet Static <$> X.getAttribute "src" e
    "vendor" -> Stylesheet Vendored <$> X.getAttribute "src" e
extractStyleSheet _ = Nothing

generateCompiled _ [] = return []
generateCompiled as assets = liftM2 (++)
    (generateCompiledScripts as (filterScript assets))
    (generateCompiledStylesheets as (filterStylesheet assets))
    where filterScript = mapMaybe (\ x -> case x of ScriptAsset s -> Just s; _ -> Nothing)
          filterStylesheet = mapMaybe (\ x -> case x of StylesheetAsset s -> Just s; _ -> Nothing)

generateCompiledScripts :: MonadIO m => Assets -> [Script] -> m [X.Node]
generateCompiledScripts _ [] = return []
generateCompiledScripts as scripts = do
    let compileKey = decodeUtf8 (digestToHexByteString (hash (B.concat $ map scriptKey scripts) :: Digest SHA1)) <> ".js"
        scriptUrl = "/compiled" </> T.unpack compileKey
        fullPath = outputDir as </> T.unpack compileKey
    scriptExists <- liftIO $ doesFileExist fullPath
    unless scriptExists $ liftIO $ do
        createDirectoryIfMissing True (outputDir as)
        writeFile fullPath $ show scripts
    return [X.Element "script" [("src", T.pack scriptUrl)] []]

generateCompiledStylesheets :: MonadIO m => Assets -> [Stylesheet] -> m [X.Node]
generateCompiledStylesheets _ [] = return []
generateCompiledStylesheets as sheets = do
    let compileKey = decodeUtf8 (digestToHexByteString (hash (B.concat $ map sheetKey sheets) :: Digest SHA1)) <> ".css"
        cssUrl = "/compiled" </> T.unpack compileKey
        fullPath = outputDir as </> T.unpack compileKey
    cssExists <- liftIO $ doesFileExist fullPath
    unless cssExists $ liftIO $ do
        createDirectoryIfMissing True (outputDir as)
        writeFile fullPath $ show sheets
    return [X.Element "link" [("type", "text/css"), ("rel", "stylesheet"), ("href", T.pack cssUrl)] []]

{-
getSources as q = mapM getScriptSource scripts where
    scriptPairs = sort $ mapMaybe (\ x -> liftM2 (,) (X.getAttribute "type" x) (X.getAttribute "src" x)) q
    scriptPairHash = hash (map (encodeUtf8 . snd) scriptPairs)
    getScriptSource ("vendor", x) = fmap LocalScript $ B.readFile $ vendorDir as </> T.unpack x
    getScriptSource ("coffee", y) = do
        (status, out, err) <- readProcessWithExitCode "coffee" ["-cp"] filepath
        case status of
            ExitSuccess -> return $ LocalScript out
            ExitFailure i -> error $ printf "Error compiling %s (shell exited %d): %s"
                (T.unpack y) i (T.unpack $ decodeUtf8 err)
        where filepath = encodeUtf8 $ T.pack (coffeeSrcDir as </> T.unpack y)
    getScriptSource ("remote", y) = return $ RemoteScript y
    getScriptSource (x, _) = error (T.unpack x)
    -}

extractTag :: Text -> X.Node -> ([X.Node], Maybe X.Node)
extractTag tname (X.Element t a c)
    | t == tname = (c, Nothing)
    | otherwise = (concat compiles, Just $ X.Element t a (catMaybes mcs))
    where
        (compiles, mcs) = unzip $ map (extractTag tname) c
extractTag _ n = ([], Just n)
