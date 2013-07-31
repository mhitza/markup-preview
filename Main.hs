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
    import Control.Monad.Trans (lift)
    import Control.Monad.Trans.Maybe

    import Paths_markup_preview

    import Debug.Trace


    generateHtml format contents = writeHtmlString def intermediaryDocument where
        intermediaryDocument = 
            let reader = fromJust $ lookup format [("Markdown", readMarkdown), ("reStructuredText", readRST), ("Textile", readTextile)]
            in reader (def { readerStandalone = True }) contents


    createFilter name filepatterns = do
        fileFilter <- G.fileFilterNew
        mapM_ (G.fileFilterAddPattern fileFilter) filepatterns
        G.fileFilterSetName fileFilter name
        return fileFilter


    createOpenDialog = do
        dialog <- G.fileChooserDialogNew 
                    (Just "Choose a markup file")
                    Nothing
                    G.FileChooserActionOpen
                    [("Ok", G.ResponseAccept), ("Cancel", G.ResponseCancel)]
        createFilter "Markdown" ["*.md", "*.markdown"] >>= G.fileChooserAddFilter dialog
        createFilter "reStructuredText" ["*.rst", "*.rest", "*.restx"] >>= G.fileChooserAddFilter dialog 
        createFilter "Textile" ["*.textile"] >>= G.fileChooserAddFilter dialog 

        return dialog


    createToolbar loadNotifier = do
        toolbar <- G.toolbarNew
        G.toolbarSetStyle toolbar G.ToolbarIcons
        openButton <- G.toolButtonNewFromStock "gtk-open"
        void $ G.onToolButtonClicked openButton $ do
            openDialog <- createOpenDialog
            dialogResponse <- G.dialogRun openDialog
            when (dialogResponse == G.ResponseAccept) $ do
                response <- runMaybeT $ do
                    filepath <- MaybeT $ G.fileChooserGetFilename openDialog
                    fileFilter <- MaybeT $  G.fileChooserGetFilter openDialog
                    format <- lift $ G.fileFilterGetName fileFilter 
                    return (format, filepath)
                print response
                when (isJust response) $ putMVar loadNotifier (fromJust response)
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
