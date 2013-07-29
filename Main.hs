module Main where

    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

    import Control.Monad (void, forever, when)
    import Control.Concurrent (forkIO)
    import Control.Concurrent.Chan
    import Data.Maybe (isJust, fromJust)

    import Text.Pandoc
    import System.Directory (getTemporaryDirectory)
    import System.IO.Temp (openTempFile)
    import GHC.IO.Handle (hPutStr, hFlush)

    import Debug.Trace

    previewRST contents = 
        let irDocument = readRST (def { readerStandalone = True }) contents
        in writeHtmlString def irDocument

    previewMarkdown contents =
        let irDocument = readMarkdown (def { readerStandalone = True }) contents
        in writeHtmlString def irDocument

    createOpenDialog = do
        dialog <- G.fileChooserDialogNew 
                    (Just "Choose a markup file")
                    Nothing
                    G.FileChooserActionOpen
                    [("Ok", G.ResponseAccept), ("Cancel", G.ResponseCancel)]
        textFileFilter <- G.fileFilterNew
        G.fileFilterSetName textFileFilter "Text files"
        G.fileFilterAddMimeType textFileFilter "text/plain"
        G.fileChooserAddFilter dialog textFileFilter
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


    createToolbar loadChannel = do
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
                        writeChan loadChannel (format, fromJust filepath)
            G.widgetDestroy openDialog

        G.toolbarInsert toolbar openButton 0

        return toolbar


    createInterface loadChannel = do
        window <- G.windowNew
        scrolledWindow <- G.scrolledWindowNew Nothing Nothing
        webView <- GW.webViewNew
        G.set scrolledWindow [ G.containerChild := webView ]
        statusBar <- G.statusbarNew
        toolBar <- createToolbar loadChannel

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
        void $ G.initGUI
        window <- f
        G.onDestroy window G.mainQuit
        G.widgetShowAll window
        G.mainGUI

    main :: IO ()
    main = withGUI $ do
        loadChannel <- newChan :: IO (Chan (String, String))
        (window, webView) <- createInterface loadChannel
        forkIO $ forever $ do
            (format, filepath) <- readChan loadChannel 
            contents <- readFile filepath
            let htmlContent = case format of "reStructuredText" -> previewRST contents
                                             "Markdown"         -> previewMarkdown contents
                                             _                  -> contents
            tempDirectory <- getTemporaryDirectory
            (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview"
            hPutStr tempHandle htmlContent >> hFlush tempHandle
            GW.webViewLoadUri webView ("file://" ++ tempFilePath)
        return window
