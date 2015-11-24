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
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString)
import           Data.ByteString.Builder                     (toLazyByteString)
import           Data.ByteString.Lazy                        (toStrict)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                   as T
import           Data.Text.Encoding
import           Data.Text.Lazy                              (fromStrict)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Interpreted                           as I
import           HighlightedMarkdown
import           Models
import           Network.HTTP.Types                          (renderQueryText)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Coffee
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Sass
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           System.Environment                          (lookupEnv)
import           Text.Markdown                               (Markdown (Markdown))
import           Web.PathPieces
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

handleIndex :: Handler App PersistState ()
handleIndex = do
    posts <- runPersist (selectList ([] :: [Filter Entry]) [])
    let urls = map (\(Entity _ post) ->
            ("2", Just $ T.pack "https://jude.bio/r/" <> entrySlug post)) posts
        qs = toLazyByteString $ renderQueryText True urls
    heistLocal (splices posts qs) $ render "home"
    where
        splices posts qs = I.bindSplices $ do
            "posts" ## I.mapSplices renderOne posts
            "disqusUrl" ## I.textSplice (decodeUtf8 $ toStrict qs)
        renderOne (Entity _ post) = I.runChildrenWithText $ do
            "postTitle" ## entryTitle post
            "postSlug" ## entrySlug post

handleReadSingle :: Handler App PersistState ()
handleReadSingle = do
    Just slug <- getParam "slug"
    Just (Entity k e) <- runPersist $ getBy (UniqueEntry $ decodeUtf8 slug)
    heistLocal (I.bindSplices $ do
        "postTitle" ## I.textSplice (entryTitle e)
        "postId" ## I.textSplice (toPathPiece k)
        "postSlug" ## I.textSplice (entrySlug e)
        "postContent" ## markdownToSplice (Markdown . fromStrict $ entryContent e)
        ) (render "single")


------------------------------------------------------------------------------
-- | The application's routes.
routes :: IO [(ByteString, Handler App App ())]
routes = do
    bowerComponents <- fromMaybe "bower_components" <$> liftIO (lookupEnv "BOWER_COMPONENTS")
    return [ ("/r/:slug",  with db handleReadSingle)
           , ("/s",        serveDirectory "static")
           , ("/css",      with sass sassServe)
           , ("/js",       with coffee coffeeServe)
           , ("/vendor",   serveDirectory bowerComponents)
           , ("/in",       with auth handleLoginSubmit)
           , ("/out",      with auth handleLogout)
           , ("/",         ifTop (with db handleIndex))
           ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
        initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "users.json"
    c <- nestSnaplet "coffee" coffee initCoffee
    ss <- nestSnaplet "sass" sass initSass
    p <- nestSnaplet "db" db (initPersist (runMigration migrateAll))

    addRoutes =<< liftIO routes
    addAuthSplices h auth
    return $ App h s a ss p c
