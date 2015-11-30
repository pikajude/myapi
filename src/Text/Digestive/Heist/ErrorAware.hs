{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Text.Digestive.Heist.ErrorAware
    ( -- * Core methods
      formSplice
    , formSplice'

      -- * Main splices
    , dfInput
    , dfInputList
    , dfInputText
    , dfInputTextArea
    , dfInputPassword
    , dfInputHidden
    , dfInputSelect
    , dfInputSelectGroup
    , dfInputRadio
    , dfInputCheckbox
    , dfInputFile
    , dfInputSubmit
    , dfLabel
    , dfErrorList
    , dfChildErrorList
    , dfSubView

      -- * Utility splices
    , dfIfChildErrors
    , digestiveSplices
    ) where


import           Blaze.ByteString.Builder
import           Control.Monad            (mplus)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Function            (on)
import           Data.List                (unionBy)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Heist
import           Heist.Compiled
import           Heist.Compiled.LowLevel
import           Text.Digestive.Form.List
import           Text.Digestive.View
import           Text.Printf
import qualified Text.XmlHtml             as X

digestiveSplices :: (MonadIO m)
                 => RuntimeSplice m (View Text)
                 -> Splices (Splice m)
digestiveSplices vp = do
    "dfInput"            ## dfInput vp
    "dfValue"            ## dfValue vp
    "dfInputText"        ## dfInputText vp
    "dfInputTextArea"    ## dfInputTextArea vp
    "dfInputPassword"    ## dfInputPassword vp
    "dfInputHidden"      ## dfInputHidden vp
    "dfInputSelect"      ## dfInputSelect vp
    "dfInputSelectGroup" ## dfInputSelectGroup vp
    "dfInputRadio"       ## dfInputRadio vp
    "dfInputCheckbox"    ## dfInputCheckbox vp
    "dfInputFile"        ## dfInputFile vp
    "dfInputSubmit"      ## dfInputSubmit
    "dfLabel"            ## dfLabel vp
    "dfErrorList"        ## dfErrorList vp
    "dfChildErrorList"   ## dfChildErrorList vp
    "dfSubView"          ## dfSubView vp
    "dfIfChildErrors"    ## dfIfChildErrors vp
    "dfIfNoChildErrors"  ## dfIfNoChildErrors vp
    "dfInputList"        ## dfInputList vp
    "dfEncType"          ## dfEncType vp

formSplice :: MonadIO m
           => Splices (Splice m)
           -> Splices (AttrSplice m)
           -> RuntimeSplice m (View Text)
           -> Splice m
formSplice ss as = formSplice' (const ss) (const as)

formSplice' :: MonadIO m
            => (RuntimeSplice m (View Text) -> Splices (Splice m))
            -> (RuntimeSplice m (View Text) -> Splices (AttrSplice m))
            -> RuntimeSplice m (View Text)
            -> Splice m
formSplice' ss as = deferMap return $ \getView -> do
    node <- getParamNode
    (_, attrs) <- getRefAttributes node (Just "")
    let tree = X.Element "form"
                 (addAttrs attrs
                    [ ("method", "POST")
                    , ("enctype", "${dfEncType}")
                    ])
                 (X.childNodes node)
        action = runNode tree
    withLocalSplices (digestiveSplices getView `mappend` ss getView) (as getView) action

attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)

setDisabled :: Text -> View v -> [(Text, Text)] -> [(Text, Text)]
setDisabled ref view = if viewDisabled ref view then (("disabled",""):) else id

getRefAttributes
    :: MonadIO m
    => X.Node
    -> Maybe Text
    -> HeistT n m (Text, [(Text, Text)])
getRefAttributes node defaultRef = do
    tfp <- getTemplateFilePath
    getRefAttributes' tfp node defaultRef

getRefAttributes'
    :: MonadIO m
    => Maybe FilePath
    -> X.Node
    -> Maybe Text
    -> m (Text, [(Text, Text)])
getRefAttributes' tfp node defaultRef = do
    let end s = do
            liftIO $ putStrLn s
            return ("", [])
    case node of
        X.Element n as _ -> do
            case lookup "ref" as `mplus` defaultRef of
              Nothing -> end $ printf "%s: missing ref, path %s"
                               (T.unpack n) (show tfp)
              Just ref -> return (ref, filter ((/= "ref") . fst) as)
        _                ->
            end $ "Wrong type of node! (" ++ show node ++ ")"

dfEncType :: (Monad m)
          => RuntimeSplice m (View v)
          -> Splice m
dfEncType getView = do
    return $ yieldRuntime $ do
        view <- getView
        return $ fromByteString $ encodeUtf8 $ T.pack (show $ viewEncType view)

dfMaster :: Monad m
         => (Text -> [(Text, Text)] -> View v -> RuntimeSplice m Builder)
         -> RuntimeSplice m (View v) -> Splice m
dfMaster f getView = do
    node <- getParamNode
    (ref, attrs) <- getRefAttributes node Nothing
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getView
        attrs' <- runAttrs
        f ref attrs' view

dfTag :: Monad m
      => (Text -> [(Text, Text)] -> Text -> [X.Node])
      -> RuntimeSplice m (View v)
      -> Splice m
dfTag f = dfMaster $ \ref attrs view -> do
    let ref' = absoluteRef ref view
        !value = fieldInputText ref view
        attrs' = setDisabled ref view attrs
        errs = childErrors ref view
        attrs'' = if null errs then attrs' else addClassError attrs'
    return $ X.renderHtmlFragment X.UTF8 $ f ref' attrs'' value

dfInputGeneric :: MonadIO m
               => [(Text, Text)]
               -> RuntimeSplice m (View v)
               -> Splice m
dfInputGeneric as = dfTag $ \ref attrs value ->
    makeElement "input" [] $ addAttrs attrs $
        as ++ [("id", ref), ("name", ref), ("value", value)]

dfInputSubmit :: Monad m => Splice m
dfInputSubmit = do
    node <- getParamNode
    (_, attrs) <- getRefAttributes node (Just "")
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        attrs' <- runAttrs
        let e = makeElement "input" [] $ addAttrs attrs'
                [("type", "submit")]
        return $ X.renderHtmlFragment X.UTF8 e

dfLabel :: Monad m => RuntimeSplice m (View v) -> Splice m
dfLabel getView = do
    node <- getParamNode
    (ref, attrs) <- getRefAttributes node Nothing
    childList <- runChildren
    runAttrs <- runAttributesRaw attrs
    return $ yieldRuntime $ do
        view <- getView
        attrs' <- runAttrs
        let ref'  = absoluteRef ref view
            errs = childErrors ref view
            attrs'' = if null errs then attrs' else addClassError attrs'
            e = makeElement "label" [] $ addAttrs attrs''
                [("for", ref')]
        bytes <- codeGen childList
        case X.parseHTML "<anonymous>" $ toStrict $ toLazyByteString bytes of
            Left parseErr -> error parseErr
            Right (X.XmlDocument{}) -> error "what?"
            Right (X.HtmlDocument { X.docContent = nodes }) ->
                return $ X.renderHtmlFragment X.UTF8 $ map (\ y -> y { X.elementChildren = nodes }) e

dfInput :: MonadIO m => RuntimeSplice m (View v) -> Splice m
dfInput = dfInputGeneric []

dfInputText :: MonadIO m => RuntimeSplice m (View v) -> Splice m
dfInputText = dfInputGeneric [("type", "text")]

dfInputTextArea :: MonadIO m => RuntimeSplice m (View v) -> Splice m
dfInputTextArea = dfTag $ \ref attrs value ->
    makeElement "textarea" [X.TextNode value] $ addAttrs attrs
        [("id", ref), ("name", ref)]

dfInputPassword :: MonadIO m => RuntimeSplice m (View v) -> Splice m
dfInputPassword = dfInputGeneric [("type", "password")]

dfInputHidden :: MonadIO m => RuntimeSplice m (View v) -> Splice m
dfInputHidden = dfInputGeneric [("type", "hidden")]

dfInputCheckbox :: Monad m
                => RuntimeSplice m (View v)
                -> Splice m
dfInputCheckbox = dfMaster $ \ref attrs view -> do
    let ref'  = absoluteRef ref view
        value = fieldInputBool ref view
        e = makeElement "input" [] $ addAttrs attrs $
                   attr value ("checked", "checked") $
                   [("type", "checkbox"), ("id", ref'), ("name", ref')]

    return $ X.renderHtmlFragment X.UTF8 e

dfInputFile :: Monad m => RuntimeSplice m (View v) -> Splice m
dfInputFile = dfMaster $ \ref attrs view -> do
    let ref'  = absoluteRef ref view
        value = maybe "" T.pack $ fieldInputFile ref view
        e = makeElement "input" [] $ addAttrs attrs $
            [ ("type", "file"), ("id", ref')
            , ("name", ref'), ("value", value)]
    return $ X.renderHtmlFragment X.UTF8 e

dfInputSelect :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputSelect = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = map makeOption choices
        value i  = ref' <> "." <> i

        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

        e = makeElement "select" kids $ addAttrs attrs
            [("id", ref'), ("name", ref')]
    return $ X.renderHtmlFragment X.UTF8 e

dfInputSelectGroup :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputSelectGroup = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoiceGroup ref view
        kids     = map makeGroup choices
        value i  = ref' <> "." <> i

        makeGroup (name, options) = X.Element "optgroup"
            [("label", name)] $ map makeOption options
        makeOption (i, c, sel) = X.Element "option"
            (attr sel ("selected", "selected") [("value", value i)])
            [X.TextNode c]

        e = makeElement "select" kids $ addAttrs attrs
            [("id", ref'), ("name", ref')]
    return $ X.renderHtmlFragment X.UTF8 e

dfInputRadio :: Monad m => RuntimeSplice m (View Text) -> Splice m
dfInputRadio = dfMaster $ \ref attrs view -> do
    let ref'     = absoluteRef ref view
        choices  = fieldInputChoice ref view
        kids     = concatMap makeOption choices
        value i  = ref' <> "." <> i

        makeOption (i, c, sel) =
            [ X.Element "input"
                (attr sel ("checked", "checked") $ addAttrs attrs
                    [ ("type", "radio"), ("value", value i)
                    , ("id", value i), ("name", ref')
                    ]) []
            , X.Element "label" [("for", value i)] [X.TextNode c]
            ]

    return $ X.renderHtmlFragment X.UTF8 kids

dfErrorList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfErrorList getView = do
    node <- getParamNode
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, attrs) <- getRefAttributes' tfp node Nothing
        let nodes = errorList (errors ref view) attrs
        return $ X.renderHtmlFragment X.UTF8 nodes

dfChildErrorList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfChildErrorList getView = do
    node <- getParamNode
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, attrs) <- getRefAttributes' tfp node (Just "")
        let nodes = errorList (childErrors ref view) attrs
        return $ X.renderHtmlFragment X.UTF8 nodes

dfIfChildErrors :: (MonadIO m) => RuntimeSplice m (View v) -> Splice m
dfIfChildErrors getView = do
    node <- getParamNode
    childrenChunks <- runChildren
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, _) <- getRefAttributes' tfp node $ Just ""
        if null (childErrors ref view)
          then return mempty
          else codeGen childrenChunks

dfIfNoChildErrors :: (MonadIO m) => RuntimeSplice m (View v) -> Splice m
dfIfNoChildErrors getView = do
    node <- getParamNode
    childrenChunks <- runChildren
    tfp <- getTemplateFilePath
    return $ yieldRuntime $ do
        view <- getView
        (ref, _) <- getRefAttributes' tfp node $ Just ""
        if null (childErrors ref view)
          then codeGen childrenChunks
          else return mempty

dfSubView :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfSubView getView = do
    node <- getParamNode
    p2 <- newEmptyPromise
    tfp <- getTemplateFilePath
    let action = yieldRuntimeEffect $ do
            view <- getView
            (ref, _) <- getRefAttributes' tfp node Nothing
            let view' = subView ref view
            putPromise p2 view'
    res <- withLocalSplices (digestiveSplices (getPromise p2)) mempty $
             runNodeList $ X.childNodes node
    return $ action <> res

dfValue :: Monad m => RuntimeSplice m (View v) -> Splice m
dfValue = dfMaster $ \ref _ view -> do
    let !value = fieldInputText ref view
    return $ fromByteString $ encodeUtf8 value

dfSingleListItem :: MonadIO n
                 => X.Node
                 -> (RuntimeSplice n (View Text) -> AttrSplice n)
                 -> RuntimeSplice n (View Text)
                 -> Splice n
dfSingleListItem node attrs viewPromise = do
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ do
            view <- viewPromise
            putPromise p2 view
    res <- withLocalSplices (digestiveSplices (getPromise p2))
                            ("itemAttrs" ## attrs viewPromise)
                            (runNodeList $ X.childNodes node)
    return $ action <> res

dfInputList :: MonadIO m => RuntimeSplice m (View Text) -> Splice m
dfInputList getView = do
    node <- getParamNode
    itemsPromise <- newEmptyPromise
    refPromise <- newEmptyPromise
    indicesPromise <- newEmptyPromise
    templateViewPromise <- newEmptyPromise
    tfp <- getTemplateFilePath
    let itemAttrs gv _ = do
            view <- gv
            listRef <- getPromise refPromise
            return
              [ ("data-ind", T.concat [listRef, ".", last $ "0" : viewContext view])
              , ("class", T.append listRef ".inputListItem")
              ]
        templateAttrs gv _ = do
            view <- gv
            listRef <- getPromise refPromise
            return
              [ ("data-ind", T.concat [listRef, ".", last $ "-1" : viewContext view])
              , ("class", T.append listRef ".inputListTemplate")
              , ("style", "display: none;")
              ]
        dfListItem = do
            n <- getParamNode
            template <- dfSingleListItem n templateAttrs (getPromise templateViewPromise)
            body <- deferMany (dfSingleListItem n itemAttrs) $
                              getPromise itemsPromise
            return $ if X.hasAttribute "noTemplate" n
              then body
              else mconcat [ yieldPureText "<div class=\"inputListInstance\">"
                           , template
                           , body
                           , yieldPureText "</div>"
                           ]
    let listAttrs =
            [ ("id", "${dfListRef}")
            , ("class", "inputList")
            ]
        indices = X.Element "input"
                    [ ("type", "hidden")
                    , ("name", T.intercalate "." ["${dfListRef}", indicesRef])
                    , ("value", "${dfIndicesList}")
                    ] []
        e = X.Element "div" listAttrs (indices : X.childNodes node)
    let addControl _ = do
            listRef <- getPromise refPromise
            return [ ("onclick", T.concat [ "addInputListItem(this, '"
                                          , listRef
                                          , "'); return false;"] ) ]
        removeControl _ = do
            listRef <- getPromise refPromise
            return [ ("onclick", T.concat [ "removeInputListItem(this, '"
                                          , listRef
                                          , "'); return false;"] ) ]
        attrSplices = do
            "addControl"    ## addControl
            "removeControl" ## removeControl
        splices = do
            "dfListRef"     ## return $ yieldRuntimeText $ getPromise refPromise
            "dfIndicesList" ## return $ yieldRuntimeText $ getPromise indicesPromise
            "dfListItem"    ## dfListItem

    let action = yieldRuntimeEffect $ do
          view <- getView
          (ref, _) <- getRefAttributes' tfp node Nothing
          let listRef  = absoluteRef ref view
              items = listSubViews ref view
              tview = makeListSubView ref (-1) view
          putPromise refPromise listRef
          putPromise indicesPromise $ T.intercalate "," $
            map (last . ("0":) . viewContext) items
          putPromise itemsPromise items
          putPromise templateViewPromise tview

    res <- withLocalSplices splices attrSplices $ runNode e
    return $ action <> res

makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes = return . flip (X.Element name) nodes

addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)

errorList :: [Text] -> [(Text, Text)] -> [X.Node]
errorList []   _ = []
errorList errs _ = map X.TextNode errs

addClassError :: [(Text, Text)] -> [(Text, Text)]
addClassError (("class", foo):xs) = ("class", foo <> " error") : xs
addClassError (x:xs) = x : addClassError xs
addClassError [] = [("class", "error")]
