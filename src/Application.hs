{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Assets
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Snaplet.Session

type AppHandler = Handler App App

data App = App
    { _heist  :: Snaplet (Heist App)
    , _sess   :: Snaplet SessionManager
    , _auth   :: Snaplet (AuthManager App)
    , _assets :: Snaplet AssetConfig
    , _db     :: Snaplet PersistState
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist
