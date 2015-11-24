{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Coffee
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Snaplet.Sass
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist  :: Snaplet (Heist App)
    , _sess   :: Snaplet SessionManager
    , _auth   :: Snaplet (AuthManager App)
    , _sass   :: Snaplet Sass
    , _db     :: Snaplet PersistState
    , _coffee :: Snaplet CoffeeScript
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
