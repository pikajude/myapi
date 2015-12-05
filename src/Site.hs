{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  -- ( app
  -- ) where
  where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.ByteString                             (ByteString)
import Data.Monoid
import Database.Persist.Sql
import Heist
import Snap.Core
import Snap.Extras.FlashNotice
import Snap.Extras.MethodOverride
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Text.Digestive.Snap                         hiding (method)
------------------------------------------------------------------------------
import Application
import Forms
import Models
import PersistUtils
import Snap.Snaplet.Assets
import Splices

handleLoginPost :: Handler App (AuthManager App) ()
handleLoginPost = do
    user <- liftM snd $ runForm "login" loginForm
    case user of
        Nothing -> cRender "login"
        Just _ -> redirect "/"

handleEditPost :: AppHandler ()
handleEditPost = do
    eKey <- getPathPiece "key"
    entry <- with db $ run404 $ get eKey
    result <- liftM snd $ runForm "entry" (entryForm $ Just entry)
    case result of
        Nothing -> cRender "edit"
        Just newEntry -> do
            with db $ runPersist $ replace eKey newEntry
            flashSuccess sess $ "Updated ‘" <> entryTitle newEntry <> "’"
            redirect $ entryPath newEntry

handleNewPost :: AppHandler ()
handleNewPost = do
    result <- liftM snd $ runForm "entry" (entryForm Nothing)
    case result of
        Nothing -> cRender "new"
        Just newEntry -> do
            _ <- with db $ runPersist $ insert newEntry
            redirect $ entryPath newEntry

handleDelete :: AppHandler ()
handleDelete = do
    eKey <- getPathPiece "key"
    e <- with db $ do
        entry <- run404 $ get eKey
        runPersist $ delete eKey
        return entry
    flashSuccess sess $ "Deleted ‘" <> entryTitle e <> "’"
    redirect "/"

routes :: [(ByteString, AppHandler ())]
routes = [ ("/r/:slug",  cRender "single")
         , ("/e/:key",   needAuth $ method GET (cRender "edit")
                                <|> method POST handleEditPost)
         , ("/d/:key",   needAuth $ handleMethodOverride $ method DELETE handleDelete)
         , ("/n",        needAuth $ method GET (cRender "new")
                                <|> method POST handleNewPost)

         , ("/in",       needNoAuth $ with auth $ method GET (cRender "login")
                                              <|> method POST handleLoginPost)
         , ("/out",      needAuth $ with auth $ logout >> redirect "/")

         , ("/s",        serveDirectory "static")

         , ("/",         ifTop $ cRender "home")

         , ("",          handle404)
         ]
    where needAuth = requireUser auth pass
          needNoAuth x = requireUser auth x (redirect "/")
          handle404 = do
              modifyResponse $ setResponseCode 404
              cRender "404"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    let hc = emptyHeistConfig & hcNamespace .~ ""
                              & hcErrorNotBound .~ True
                              & hcSpliceConfig .~ sc
        sc = mempty & scLoadTimeSplices .~ defaultLoadTimeSplices
                    & scCompiledSplices .~ siteSplices
    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    s <- nestSnaplet "sess" sess $
        initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "users.json"
    p <- nestSnaplet "db" db (initPersist (runMigration migrateAll))
    ac <- nestSnaplet "assets" assets (initAssets h Nothing)

    addAuthSplices h auth
    addAssetRoutes ac
    initFlashNotice h sess
    addRoutes routes
    return $ App h s a ac p
