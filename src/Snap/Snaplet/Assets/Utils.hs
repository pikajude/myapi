module Snap.Snaplet.Assets.Utils where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as B
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process.ByteString
import           Text.Printf

ensureFileWithContents :: MonadIO m => FilePath -> IO B.ByteString -> m ()
ensureFileWithContents fp act = do
    e <- liftIO $ doesFileExist fp
    unless e $ liftIO $ do
        createDirectoryIfMissing True (takeDirectory fp)
        contents <- act
        B.writeFile fp contents

runToolWith :: MonadIO m => FilePath -> [String] -> B.ByteString -> m B.ByteString
runToolWith tool args input = liftIO $ do
    (status, out, err) <- readProcessWithExitCode tool args input
    case status of
        ExitSuccess -> return out
        ExitFailure i -> error $ printf "Error reported from '%s' (shell exited %d): %s" tool i (T.unpack $ decodeUtf8 err)
