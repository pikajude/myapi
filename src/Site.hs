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
import           Control.Monad.Logger                        (NoLoggingT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource                (ResourceT)
import           Data.ByteString                             (ByteString)
import           Data.ByteString.Builder                     (Builder, toLazyByteString)
import           Data.ByteString.Lazy                        (toStrict)
import           Data.Char                                   (isAlphaNum)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                   as T
import           Data.Text.Encoding
import           Data.Text.Lazy                              (fromStrict)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                              as C
import qualified Heist.Interpreted                           as I
import           Heist.Splices.Html
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
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Text.Markdown                               (Markdown (Markdown))
import           Web.PathPieces
------------------------------------------------------------------------------
import           Application

type RSplice = RuntimeSplice (Handler App App)

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

handleReadSingle :: Handler App PersistState ()
handleReadSingle = do
    Just slug <- getParam "slug"
    e <- run404 $ getBy (UniqueEntry $ decodeUtf8 slug)
    undefined -- heistLocal (I.bindSplices $ postSplices e) (render "single")

singleSplices :: Splices (C.Splice (Handler App App))
singleSplices = "singleEntry" ## withCache getSinglePost (C.withSplices C.runChildren postSplices)
    where
        getSinglePost = lift $ do
            slug <- maybe pass return =<< getParam "slug"
            withTop db $ run404 $ getBy (UniqueEntry $ decodeUtf8 slug)

homepageSplices :: Splices (C.Splice (Handler App App))
homepageSplices = "homePage" ## withCache getAllPosts (C.withSplices C.runChildren $ do
        "postList" ## (C.manyWithSplices C.runChildren postSplices)
        "disqusUrl" ## C.pureSplice (\ es ->
            let urls = map (\(Entity _ post) ->
                    ("2", Just $ T.pack "https://jude.bio/r/" <> entrySlug post)) es
                qs = renderQueryText True urls
             in "//otters.disqus.com/count-data.js" <> qs
                       )
        )
    where
        getAllPosts :: RuntimeSplice (Handler App App) [Entity Entry]
        getAllPosts = lift $ withTop db $ runPersist (selectList [] [])

withCache x y = C.deferMap return y x

postSplices :: Splices (RSplice (Entity Entry) -> C.Splice (Handler App App))
postSplices = mapV C.pureSplice $ do
    "postTitle" ## C.textSplice (entryTitle . entityVal)
    "postId" ## C.textSplice (toPathPiece . entityKey)
    "postSlug" ## C.textSplice (entrySlug . entityVal)
    "postContent" ## C.htmlNodeSplice (markdownToSplice . Markdown . fromStrict . entryContent . entityVal)
    "postContentRaw" ## C.textSplice (entryContent . entityVal)

entryForm :: Monad m => Maybe Entry -> Form T.Text m Entry
entryForm mentry = (\ a b c -> Entry a (mkSlug a) b c)
    <$> "title" .: check "Title can't be empty" (not . T.null) (text (entryTitle <$> mentry))
    <*> "content" .: text (entryContent <$> mentry)
    <*> "createdAt" .: stringRead "Created at must be a date" (entryCreatedAt <$> mentry)
    where
        mkSlug = T.pack . trim . squash . map dasherize . T.unpack . T.toLower
        dasherize x = if isAlphaNum x then x else '-'
        trim = dropWhile (== '-') . reverse . dropWhile (== '-') . reverse
        squash ('-':'-':xs) = '-' : squash xs
        squash (x:xs) = x : squash xs
        squash [] = []

handleEdit :: Handler App PersistState ()
handleEdit = do
    Just k <- getParam "key"
    let eKey = fromJust . fromPathPiece $ decodeUtf8 k
    entry <- run404 $ get eKey
    (view, result) <- runForm "entry" (entryForm $ Just entry)
    case result of
        Just x -> writeText $ T.pack $ show x
        Nothing ->
            let splices = do
                    digestiveSplices view
                    -- postSplices (Entity eKey entry)
                    "formAction" ## I.textSplice ("/e/" <> toPathPiece eKey)
             in heistLocal (I.bindSplices splices) $ render "edit"

run404 :: SqlPersistT (ResourceT (NoLoggingT IO)) (Maybe b) -> Handler App PersistState b
run404 = maybe pass return <=< runPersist

routes :: IO [(ByteString, Handler App App ())]
routes = do
    bowerComponents <- fromMaybe "bower_components" <$> liftIO (lookupEnv "BOWER_COMPONENTS")
    return [ ("/r/:slug",  cRender "single")
           -- , ("/e/:key",   with db handleEdit)
           , ("/s",        serveDirectory "static")
           , ("/css",      with sass sassServe)
           , ("/js",       with coffee coffeeServe)
           , ("/vendor",   serveDirectory bowerComponents)
           -- , ("/in",       with auth handleLoginSubmit)
           -- , ("/out",      with auth handleLogout)
           -- , ("/",         ifTop (with db handleIndex))
           , ("/",         ifTop (cRender "home"))
           ]

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
                    & scCompiledSplices .~ homepageSplices <> singleSplices
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
