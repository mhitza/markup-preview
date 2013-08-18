{-# LANGUAGE CPP #-}
module Application.GUI (withGUI, createInterface, loadHtmlInView) where


    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

#ifdef CABAL
    import Paths_markup_preview
#endif

    import Control.Monad (void, when)
    import Control.Monad.Trans (lift)
    import Control.Monad.Trans.Maybe
    import Data.Maybe (isJust, fromJust)
    import Control.Concurrent.MVar
    import GHC.IO.Handle (hPutStr, hFlush)
    import System.Directory (getTemporaryDirectory)
    import System.IO.Temp (openTempFile)
    import Text.Pandoc (renderTemplate)


    createFilter :: String -> [String] -> IO G.FileFilter
    createFilter name filepatterns = do
        fileFilter <- G.fileFilterNew
        mapM_ (G.fileFilterAddPattern fileFilter) filepatterns
        G.fileFilterSetName fileFilter name
        return fileFilter


    createOpenDialog :: IO G.FileChooserDialog
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


    createToolbar :: MVar (String, FilePath) -> IO G.Toolbar
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
                when (isJust response) $ putMVar loadNotifier (fromJust response)
            G.widgetDestroy openDialog

        G.toolbarInsert toolbar openButton 0

        return toolbar


    createInterface :: MVar (String, FilePath) -> IO (G.Window, GW.WebView)
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


    readResource :: FilePath -> IO String
#ifdef CABAL
    readResource filepath = getDataFileName filepath >>= \filepath' -> readFile filepath'
#endif
#ifndef CABAL
    readResource filepath = readFile filepath
#endif

    loadHtmlInView :: GW.WebViewClass self => self -> [Char] -> IO ()
    loadHtmlInView webView htmlContent = do
            tempDirectory <- getTemporaryDirectory
            layout <- readResource "Resources/layout.html"
            let htmlContent' = renderTemplate [("htmlContent", htmlContent)] layout
            (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview.html"
            hPutStr tempHandle htmlContent' >> hFlush tempHandle
            GW.webViewLoadUri webView ("file://" ++ tempFilePath)


    withGUI :: G.WidgetClass self => IO self -> IO ()
    withGUI f = do
        void G.initGUI
        window <- f
        void $ G.onDestroy window G.mainQuit
        G.widgetShowAll window
        G.mainGUI
