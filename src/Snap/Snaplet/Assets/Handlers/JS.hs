{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

module Snap.Snaplet.Assets.Handlers.JS where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Snap.Snaplet.Assets.API
import           Snap.Snaplet.Assets.Utils
import           Text.XmlHtml

assetsJS :: Assets
assetsJS = Assets
         { assetType = "javascript"
         , fileExtension = "js"
         , contentType = "application/javascript"
         , toNode = glt
         , compiler = c
         , minifier = runToolWith "uglifyjs" ["-c", "-m"]
         }
         where c "js" = Nothing
               c "coffee" = Just $ runToolWith "coffee" ["-cp", "/dev/stdin"]
               c l = error $ "Unknown JS source language " ++ T.unpack l

glt :: Either FilePath Text -> Node
glt (Right src) = Element "script" [("type", "text/javascript")] [TextNode src]
glt (Left path) = Element "script" [("type", "text/javascript"), ("src", T.pack path)] []
