{-# LANGUAGE OverloadedStrings #-}

module Forms (entryForm, loginForm, spliceForm) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Char              (isAlphaNum)
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Time
import           Heist                  (RuntimeSplice)
import           Snap
import           Snap.Snaplet.Auth
import           Text.Digestive
import           Text.Digestive.Snap
------------------------------------------------------------------------------
import           Application
import           Models

spliceForm :: MonadSnap m => T.Text -> Form T.Text m a -> RuntimeSplice m (View T.Text)
spliceForm formName = lift . liftM fst . runForm formName

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
        toResult (Left _) = Error "Unknown username or password."
        toResult (Right x) = Success x
