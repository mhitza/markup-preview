module Main where

    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

    import Control.Monad (void, forever, when)
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.MVar
    import Data.Maybe (isJust, fromJust)

    import Text.Pandoc
    import System.Directory (getTemporaryDirectory, getModificationTime)
    import System.IO.Temp (openTempFile)
    import GHC.IO.Handle (hPutStr, hFlush)

    import Paths_markup_preview

    import Debug.Trace


    previewRST contents = 
        let irDocument = readRST (def { readerStandalone = True }) contents
        in writeHtmlString def irDocument


    previewTextile contents =
        let irDocument = readTextile (def { readerStandalone = True }) contents
        in writeHtmlString def irDocument


    previewMarkdown contents =
        let irDocument = readMarkdown (def { readerStandalone = True }) contents
        in writeHtmlString def irDocument


    generateHtml format contents = case format of "reStructuredText" -> previewRST contents
                                                  "Textile"          -> previewTextile contents
                                                  "Markdown"         -> previewMarkdown contents
                                                  _                  -> contents


    createOpenDialog = do
        dialog <- G.fileChooserDialogNew 
                    (Just "Choose a markup file")
                    Nothing
                    G.FileChooserActionOpen
                    [("Ok", G.ResponseAccept), ("Cancel", G.ResponseCancel)]
        markdownFilter <- G.fileFilterNew
        G.fileFilterSetName markdownFilter "Markdown"
        G.fileFilterAddPattern markdownFilter "*.md"
        G.fileFilterAddPattern markdownFilter "*.markdown"
        G.fileChooserAddFilter dialog markdownFilter
        reStructuredTextFilter <- G.fileFilterNew
        G.fileFilterSetName reStructuredTextFilter "reStructuredText"
        G.fileFilterAddPattern reStructuredTextFilter "*.rst"
        G.fileFilterAddPattern reStructuredTextFilter "*.rest"
        G.fileFilterAddPattern reStructuredTextFilter "*.restx"
        G.fileChooserAddFilter dialog reStructuredTextFilter
        textileFilter <- G.fileFilterNew
        G.fileFilterSetName textileFilter "Textile"
        G.fileFilterAddPattern textileFilter "*.textile"
        G.fileChooserAddFilter dialog textileFilter

        return dialog


    createToolbar loadNotifier = do
        toolbar <- G.toolbarNew
        G.toolbarSetStyle toolbar G.ToolbarIcons
        openButton <- G.toolButtonNewFromStock "gtk-open"
        void $ G.onToolButtonClicked openButton $ do
            openDialog <- createOpenDialog
            dialogResponse <- G.dialogRun openDialog
            when (dialogResponse == G.ResponseAccept) $ do
                filepath <- G.fileChooserGetFilename openDialog
                when (isJust filepath) $ do
                    fileFilter <- G.fileChooserGetFilter openDialog
                    when (isJust fileFilter) $ do
                        format <- G.fileFilterGetName $ fromJust fileFilter 
                        putMVar loadNotifier (format, fromJust filepath)
            G.widgetDestroy openDialog

        G.toolbarInsert toolbar openButton 0

        return toolbar


    createInterface loadNotifier = do
        window <- G.windowNew
        scrolledWindow <- G.scrolledWindowNew Nothing Nothing
        webView <- GW.webViewNew
        G.set scrolledWindow [ G.containerChild := webView ]
        statusBar <- G.statusbarNew
        toolBar <- createToolbar loadNotifier

        singleColumn <- G.vBoxNew False 1
        G.boxPackStart singleColumn toolBar G.PackNatural 0
        G.boxPackStart singleColumn scrolledWindow G.PackGrow 0
        G.boxPackEnd singleColumn statusBar G.PackNatural 0

        G.set window [ G.containerChild := singleColumn
                     , G.windowDefaultWidth := 600
                     , G.windowDefaultHeight := 600
                     , G.containerBorderWidth := 1
                     ]
        return (window, webView)


    withGUI f = do
        void G.initGUI
        window <- f
        G.onDestroy window G.mainQuit
        G.widgetShowAll window
        G.mainGUI


    loadHtmlInView webView htmlContent = do
            tempDirectory <- getTemporaryDirectory
            layout <- getDataFileName "Resources/layout.html" >>= \filepath -> readFile filepath
            let htmlContent' = renderTemplate [("htmlContent", htmlContent)] layout
            (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview.html"
            hPutStr tempHandle htmlContent' >> hFlush tempHandle
            GW.webViewLoadUri webView ("file://" ++ tempFilePath)


    loaderReloader webView loadNotifier Nothing Nothing = do
        threadDelay 500
        (format, filepath) <- takeMVar loadNotifier
        modificationTime <- getModificationTime filepath
        contents <- readFile filepath
        loadHtmlInView webView $ generateHtml format contents
        loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime)

    loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime) = do
        threadDelay 500
        noNewFile <- isEmptyMVar loadNotifier
        if noNewFile
            then do modificationTime' <- getModificationTime filepath
                    when (modificationTime' /= modificationTime) $ do
                        contents <- readFile filepath
                        loadHtmlInView webView $ generateHtml format contents
                    loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime')
            else loaderReloader webView loadNotifier Nothing Nothing


    main :: IO ()
    main = withGUI $ do
        loadNotifier <- newEmptyMVar :: IO (MVar (String, String))
        (window, webView) <- createInterface loadNotifier
        forkIO $ loaderReloader webView loadNotifier Nothing Nothing
        return window
