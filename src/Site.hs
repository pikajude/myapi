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
import Control.Monad.IO.Class
import Data.ByteString                             (ByteString)
import Data.Maybe
import Data.Monoid
import Database.Persist.Sql
import Heist
import Heist.Splices.Html
import Models
import Snap.Core
import Snap.Extras.FlashNotice
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Coffee
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Snaplet.Sass
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Splices
import System.Environment                          (lookupEnv)
import Text.Digestive.Snap                         hiding (method)
------------------------------------------------------------------------------
import Application

handleLoginPost :: Handler App (AuthManager App) ()
handleLoginPost = do
    user <- liftM snd $ runForm "login" loginForm
    case user of
        Nothing -> cRender "login"
        Just _ -> redirect "/"

handleEditPost :: Handler App App ()
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

handleNewPost :: Handler App App ()
handleNewPost = do
    result <- liftM snd $ runForm "entry" (entryForm Nothing)
    case result of
        Nothing -> cRender "new"
        Just newEntry -> do
            with db $ runPersist $ insert newEntry
            flashSuccess sess $ "Created ‘" <> entryTitle newEntry <> "’"
            redirect $ entryPath newEntry

handleDelete :: Handler App App ()
handleDelete = do
    eKey <- getPathPiece "key"
    e <- with db $ do
        entry <- run404 $ get eKey
        runPersist $ delete eKey
        return entry
    flashSuccess sess $ "Deleted ‘" <> entryTitle e <> "’"
    redirect "/"

routes :: IO [(ByteString, Handler App App ())]
routes = do
    bowerComponents <- fromMaybe "bower_components" <$> lookupEnv "BOWER_COMPONENTS"
    return [ ("/r/:slug",  cRender "single")
           , ("/e/:key",   needAuth $ method GET (cRender "edit")
                                  <|> method POST handleEditPost)
           , ("/d/:key",   needAuth $ method POST handleDelete)
           , ("/n",        needAuth $ method GET (cRender "new")
                                  <|> method POST handleNewPost)

           , ("/in",       needNoAuth $ with auth $ method GET (cRender "login")
                                                <|> method POST handleLoginPost)
           , ("/out",      needAuth $ with auth $ logout >> redirect "/")

           , ("/s",        serveDirectory "static")
           , ("/css",      with sass sassServe)
           , ("/js",       with coffee coffeeServe)
           , ("/vendor",   serveDirectory bowerComponents)

           , ("/",         ifTop (cRender "home"))
           ]
    where needAuth = requireUser auth pass
          needNoAuth x = requireUser auth x (redirect "/")

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    let hc = emptyHeistConfig & hcNamespace .~ ""
                              & hcErrorNotBound .~ True
                              & hcSpliceConfig .~ sc
        sc = mempty & scLoadTimeSplices .~ do
                          defaultLoadTimeSplices
                          htmlTag ## htmlImpl
                    & scCompiledSplices .~ siteSplices
    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    s <- nestSnaplet "sess" sess $
        initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "users.json"
    c <- nestSnaplet "coffee" coffee initCoffee
    ss <- nestSnaplet "sass" sass initSass
    p <- nestSnaplet "db" db (initPersist (runMigration migrateAll))

    addRoutes =<< liftIO routes
    addAuthSplices h auth
    initFlashNotice h sess
    return $ App h s a ss p c
