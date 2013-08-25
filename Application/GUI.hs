module Application.GUI (withGUI, createInterface, handleResource) where

    import Application.Types

    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.WebKit.WebView
    import System.Directory

    import Control.Monad
    import Control.Monad.Trans
    import Control.Monad.Trans.Maybe
    import Control.Concurrent


    handleSignalOnce :: GObjectClass obj => ((t -> IO ()) -> IO (ConnectId obj)) -> t1 -> (t1 -> IO a) -> IO ()
    handleSignalOnce signalSetup callbackData callback = do
        let letItEnd mv = putMVar mv True
        m <- newEmptyMVar
        connectId <- signalSetup $ \_ -> callback callbackData >> letItEnd m
        void . forkIO $ takeMVar m >> signalDisconnect connectId
        return ()


    handleResource :: WebViewClass obj => obj -> String -> IO ()
    handleResource webView path = do
        webViewLoadUri webView path
        void . forkIO $ handleSignalOnce (on webView loadFinished) path $ \p -> removeFile (drop 7 p) -- remove 'file://'; ugly hack I repent


    createFilter :: String -> [String] -> IO FileFilter
    createFilter name filepatterns = do
        fileFilter <- fileFilterNew
        mapM_ (fileFilterAddPattern fileFilter) filepatterns
        fileFilterSetName fileFilter name
        return fileFilter


    createOpenDialog :: IO FileChooserDialog
    createOpenDialog = do
        dialog <- fileChooserDialogNew 
                    (Just "Choose a markup file")
                    Nothing
                    FileChooserActionOpen
                    [("Ok", ResponseAccept), ("Cancel", ResponseCancel)]
        createFilter "Markdown" ["*.md", "*.markdown"] >>= fileChooserAddFilter dialog
        createFilter "reStructuredText" ["*.rst", "*.rest", "*.restx"] >>= fileChooserAddFilter dialog 
        createFilter "Textile" ["*.textile"] >>= fileChooserAddFilter dialog 

        return dialog


    createToolbar :: MVar (FileType, FilePath) -> IO Toolbar
    createToolbar loadNotifier = do
        toolbar <- toolbarNew
        toolbarSetStyle toolbar ToolbarIcons
        openButton <- toolButtonNewFromStock "gtk-open"
        void $ onToolButtonClicked openButton $ do
            openDialog <- createOpenDialog
            dialogResponse' <- dialogRun openDialog
            when (dialogResponse' == ResponseAccept) $ void . runMaybeT $ do
                filepath <- MaybeT $ fileChooserGetFilename openDialog
                fileFilter <- MaybeT $  fileChooserGetFilter openDialog
                format <- lift $ fileFilterGetName fileFilter 
                lift $ putMVar loadNotifier (read format, filepath)
            widgetDestroy openDialog

        toolbarInsert toolbar openButton 0

        return toolbar


    createInterface :: MVar (FileType, FilePath) -> IO (Window, WebView)
    createInterface loadNotifier = do
        window <- windowNew
        scrolledWindow <- scrolledWindowNew Nothing Nothing
        webView <- webViewNew
        set scrolledWindow [ containerChild := webView ]
        statusBar <- statusbarNew
        toolBar <- createToolbar loadNotifier

        singleColumn <- vBoxNew False 1
        boxPackStart singleColumn toolBar PackNatural 0
        boxPackStart singleColumn scrolledWindow PackGrow 0
        boxPackEnd singleColumn statusBar PackNatural 0

        set window [ containerChild := singleColumn
                     , windowDefaultWidth := 600
                     , windowDefaultHeight := 600
                     , containerBorderWidth := 1
                     ]
        return (window, webView)


    withGUI :: WidgetClass self => IO self -> IO ()
    withGUI f = do
        void initGUI
        window <- f
        void $ onDestroy window mainQuit
        widgetShowAll window
        mainGUI
