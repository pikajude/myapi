{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.ByteString     (ByteString)
import Data.Monoid
import Data.Text           (Text)
import Data.Text.Encoding
import Data.Time
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    title Text sqltype=TEXT
    slug Text sqltype=TEXT
    content Text sqltype=TEXT
    createdAt UTCTime
    UniqueEntry slug
    deriving Show
|]

entryPath :: Entry -> ByteString
entryPath e = "/r/" <> encodeUtf8 (entrySlug e)
