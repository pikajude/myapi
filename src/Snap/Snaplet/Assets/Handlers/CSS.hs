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

module Snap.Snaplet.Assets.Handlers.CSS where

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Snap.Snaplet.Assets.API
import           Snap.Snaplet.Assets.Utils
import           Text.XmlHtml

assetsCSS :: Assets
assetsCSS = Assets
          { assetType = "css"
          , fileExtension = "css"
          , contentType = "text/css"
          , toNode = glt
          , compiler = c
          , minifier = runToolWith "cssmin" []
          }
          where c "css" = Nothing
                c "sass" = Just $ runToolWith "sass" []
                c l = error $ "Unknown CSS source language " ++ T.unpack l

glt :: Either FilePath Text -> Node
glt (Right src) = Element "style" [("type", "text/css")] [TextNode src]
glt (Left path) = Element "link" [("rel", "stylesheet"), ("type", "text/css"), ("href", T.pack path)] []
