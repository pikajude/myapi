module HighlightedMarkdown where

import qualified Data.ByteString               as BS
import           Data.Default
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Data.Text.Lazy                (toStrict)
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              (toHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Highlighter              hiding (Entity)
import           Text.Markdown
import           Text.Regex.PCRE.Light
import qualified Text.XmlHtml                  as X

markdownToSplice :: Monad m => Markdown -> m [X.Node]
markdownToSplice (Markdown m) = return nodelist where
    nodelist = X.docContent $ either error id $ X.parseHTML "<anonymous>" document
    document = encodeUtf8 $ toStrict $ renderHtml $ markdown defWithHighlight m
    defWithHighlight = def { msBlockCodeRenderer = rendered }
    pickLexer cod = fromMaybe textLexer $ lookup (maybe "text" T.unpack cod) fixedLexers
    fixedLexers = map (\ (_,x) -> (head (lAliases x), x)) lexers
    textLexer = Lexer "text" [] [] [] [Match ".*" Text Continue] [dotall]
    renderer l (tx,_) = case runLexer (pickLexer l) (encodeUtf8 tx) of
        Left es -> [Token Text $ encodeUtf8 $ T.pack $ "parse error: " ++ show es]
        Right ts -> ts
    rendered ah bh = H.figure . decentFormat $ renderer ah bh
    decentFormat ts = H.table H.! A.class_ "highlight-table" $ H.tr $ do
        H.td H.! A.class_ "linenos" $
            H.div H.! A.class_ "lineno" $
                H.pre (lineNos (countLines ts))

        H.td H.! A.class_ "code" $
            H.div H.! A.class_ "highlight" $
                H.pre $ highlight ts
    countLines [] = 0
    countLines (Token _ s:ts) =
        -- elemIndices 10 counts the number of newlines (char 10)
        -- in the string
        length (BS.elemIndices 0x0A s) + countLines ts
    highlight [] = return ()
    highlight (Token t s:ts) = do
        H.span H.! A.class_ (H.toValue $ shortName t) $ toHtml (decodeUtf8 s)
        highlight ts
    lineNos n = lineNos' 1 where
        lineNos' c
            | c - 1 <= n = do
                toHtml (show c)
                toHtml (asText "\n")
                lineNos' (c + 1)
            | otherwise = return ()
    asText :: Text -> Text
    asText = id
