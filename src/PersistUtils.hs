module PersistUtils (
    getPathPiece,
    run404
) where

import Application
import Control.Monad
import Control.Monad.Logger         (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString              (ByteString)
import Data.Text.Encoding
import Database.Persist.Sql         (SqlPersistT)
import Snap
import Snap.Snaplet.Persistent
import Web.PathPieces

run404 :: SqlPersistT (ResourceT (NoLoggingT IO)) (Maybe b) -> Handler App PersistState b
run404 = maybe pass return <=< runPersist

getPathPiece :: (MonadSnap m, PathPiece b) => ByteString -> m b
getPathPiece = maybe pass return . (>>= fromPathPiece . decodeUtf8) <=< getParam
