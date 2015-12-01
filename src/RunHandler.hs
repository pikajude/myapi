module RunHandler (runSelf) where

import           Application
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Lazy.Builder
import qualified Data.Text                    as T
import           Snap
import           Snap.Internal.Http.Types
import qualified Snap.Iteratee                as I
import           Snap.Snaplet.Test
import qualified Snap.Test                    as S (get)

runSelf :: ByteString -> AppHandler L.ByteString
runSelf pth = do
    self <- getsSnapletState (view (snapletValue . sn))
    routes <- getsSnapletState (view (snapletValue . appRoutes))
    (s, i) <- foo $ getSnaplet Nothing self
    resp <- foo $ runHandler' s i (S.get pth mempty) $ route routes
    builders <- liftIO $ either (error . show) return =<< I.run (rspBodyToEnum (rspBody resp) I.$$ I.consume)
    return $ toLazyByteString $ mconcat builders
    where foo x = x >>= either (error . T.unpack) return
