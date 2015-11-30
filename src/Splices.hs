module Splices (entryForm, run404, siteSplices, getPathPiece, loginForm) where

import           Application
import           Control.Monad
import           Control.Monad.Logger            (NoLoggingT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource    (ResourceT)
import           Data.ByteString                 (ByteString)
import           Data.Char                       (isAlphaNum)
import           Data.Monoid
import qualified Data.Text                       as T
import           Data.Text.Encoding
import           Data.Text.Lazy                  (fromStrict)
import           Data.Time
import           Database.Persist
import           Database.Persist.Sql            (SqlPersistT)
import           Heist
import qualified Heist.Compiled                  as C
import           HighlightedMarkdown
import           Models
import           Network.HTTP.Types
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Text.Digestive
import           Text.Digestive.Heist.ErrorAware
import           Text.Digestive.Snap
import           Text.Markdown                   (Markdown (Markdown))
import           Web.PathPieces

type RSplice = RuntimeSplice (Handler App App)

run404 :: SqlPersistT (ResourceT (NoLoggingT IO)) (Maybe b) -> Handler App PersistState b
run404 = maybe pass return <=< runPersist

withCache :: RSplice a -> (RSplice a -> C.Splice (Handler App App)) -> C.Splice (Handler App App)
withCache = flip (C.deferMap return)

entryForm :: MonadIO m => Maybe Entry -> Form T.Text m Entry
entryForm mentry = monadic $ do
    t <- liftIO getCurrentTime
    return $ (\ a b -> Entry a (mkSlug a) b t)
        <$> "title" .: check "Title can't be empty" (not . T.null) (text (entryTitle <$> mentry))
        <*> "content" .: check "Content can't be empty" (not . T.null) (text (entryContent <$> mentry))
    where
        mkSlug = T.pack . trim . squash . map dasherize . T.unpack . T.toLower
        dasherize x = if isAlphaNum x then x else '-'
        trim = dropWhile (== '-') . reverse . dropWhile (== '-') . reverse
        squash ('-':'-':xs) = '-' : squash xs
        squash (x:xs) = x : squash xs
        squash [] = []

loginForm :: Form T.Text (Handler App v) AuthUser
loginForm = validateM checkUserValid $
    (,,) <$> "username" .: text Nothing
         <*> "password" .: text Nothing
         <*> "remember" .: bool Nothing
    where
        checkUserValid (user, password, remember) =
            fmap toResult $ withTop auth $ loginByUsername user (ClearText $ encodeUtf8 password) remember
        toResult (Left _) = Text.Digestive.Error "Unknown username or password."
        toResult (Right x) = Success x

loginSplices :: Splices (C.Splice (Handler App App))
loginSplices = "loginForm" ## formSplice mempty mempty (lift $ liftM fst $ runForm "login" loginForm)

postSplices :: Splices (RSplice (Entity Entry) -> C.Splice (Handler App App))
postSplices = mapV C.pureSplice $ do
    "postTitle" ## C.textSplice (entryTitle . entityVal)
    "postId" ## C.textSplice (toPathPiece . entityKey)
    "postSlug" ## C.textSplice (entrySlug . entityVal)
    "postContent" ## C.htmlNodeSplice (markdownToSplice . Markdown . fromStrict . entryContent . entityVal)
    "postContentRaw" ## C.textSplice (entryContent . entityVal)

singleSplices :: Splices (C.Splice (Handler App App))
singleSplices = "singleEntry" ## withCache getSinglePost (C.withSplices C.runChildren postSplices)
    where
        getSinglePost = lift $ do
            slug <- maybe pass return =<< getParam "slug"
            withTop db $ run404 $ getBy (UniqueEntry $ decodeUtf8 slug)

newSplices :: Splices (C.Splice (Handler App App))
newSplices = "newEntry" ## (C.withSplices C.runChildren (do
     "entryForm" ## formSplice mempty mempty . (spliceForm =<<)
     "formAction" ## C.pureSplice (C.textSplice (const "/n"))
     )
    (return Nothing))

editSplices :: Splices (C.Splice (Handler App App))
editSplices = "editEntry" ## withCache getKeyedPost (C.withSplices C.runChildren $ do
        postSplices
        "entryForm" ## formSplice mempty mempty . (spliceForm . Just . entityVal =<<)
        "formAction" ## C.pureSplice (C.textSplice (("/e/" <>) . toPathPiece . entityKey)))
    where
        getKeyedPost = lift $ do
            key <- getPathPiece "key"
            fmap (Entity key) $ withTop db $ run404 $ get key

getPathPiece :: (MonadSnap m, PathPiece b) => ByteString -> m b
getPathPiece = maybe pass return . (>>= fromPathPiece . decodeUtf8) <=< getParam

spliceForm :: Maybe Entry -> RSplice (View T.Text)
spliceForm = lift . liftM fst . runForm "entry" . entryForm

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
    where getAllPosts = lift $ withTop db $ runPersist (selectList [] [Desc EntryCreatedAt])

siteSplices :: Splices (C.Splice (Handler App App))
siteSplices = mconcat [homepageSplices, singleSplices, editSplices, newSplices, loginSplices]
