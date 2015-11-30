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
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString)
import           Data.ByteString.Builder                     (toLazyByteString)
import           Data.ByteString.Lazy                        (toStrict)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                   as T
import           Data.Text.Encoding
import           Database.Persist.Sql
import           Heist
import qualified Heist.Interpreted                           as I
import           Heist.Splices.Html
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
import           Splices
import           System.Environment                          (lookupEnv)
import           Text.Digestive.Snap                         hiding (method)
import           Web.PathPieces
------------------------------------------------------------------------------
import           Application

handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

handleIndex :: Handler App PersistState ()
handleIndex = do
    posts <- runPersist (selectList ([] :: [Filter Entry]) [])
    let urls = map (\(Entity _ post) ->
            ("2", Just $ T.pack "https://jude.bio/r/" <> entrySlug post)) posts
        qs = decodeUtf8 $ toStrict $ toLazyByteString $ renderQueryText True urls
    heistLocal (splices posts qs) $ render "home"
    where
        splices posts qs = I.bindSplices $ do
            "posts" ## I.mapSplices renderOne posts
            "disqusUrl" ## I.textSplice ("//otters.disqus.com/count-data.js" <> qs)
        renderOne (Entity _ post) = I.runChildrenWithText $ do
            "postTitle" ## entryTitle post
            "postSlug" ## entrySlug post

handleEdit :: Handler App PersistState ()
handleEdit = do
    m <- rqMethod <$> getRequest
    case m of
        GET -> cRender "edit"
        POST -> do
            eKey <- getPathPiece "key"
            entry <- run404 $ get eKey
            result <- liftM snd $ runForm "entry" (entryForm $ Just entry)
            case result of
                Nothing -> cRender "edit"
                Just newEntry -> do
                    runPersist $ replace eKey newEntry
                    redirect $ "/r/" <> encodeUtf8 (entrySlug newEntry)
            maybe (cRender "edit") (writeText . T.pack . show) result
        _ -> pass

handleDelete :: Handler App PersistState ()
handleDelete = do
    eKey <- getPathPiece "key"
    runPersist $ delete (eKey :: Key Entry)
    redirect "/"

getPathPiece :: (MonadSnap m, PathPiece b) => ByteString -> m b
getPathPiece = maybe pass return . (>>= fromPathPiece . decodeUtf8) <=< getParam

routes :: IO [(ByteString, Handler App App ())]
routes = do
    bowerComponents <- fromMaybe "bower_components" <$> lookupEnv "BOWER_COMPONENTS"
    return [ ("/r/:slug",  cRender "single")
           , ("/e/:key",   needAuth $ with db handleEdit)
           , ("/d/:key",   needAuth $ with db $ method POST handleDelete)
           , ("/s",        serveDirectory "static")
           , ("/css",      with sass sassServe)
           , ("/js",       with coffee coffeeServe)
           , ("/vendor",   serveDirectory bowerComponents)
           , ("/in",       with auth handleLoginSubmit)
           , ("/out",      needAuth $ with auth handleLogout)
           , ("/",         ifTop (cRender "home"))
           ]
    where needAuth = requireUser auth pass

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
    return $ App h s a ss p c
