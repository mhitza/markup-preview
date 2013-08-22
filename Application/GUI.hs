module Application.GUI (withGUI, createInterface, webViewLoadHtmlString, webViewLoadString) where

    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.WebKit.WebView

    import Control.Monad
    import Control.Monad.Trans
    import Control.Monad.Trans.Maybe
    import Data.Maybe
    import Control.Concurrent.MVar


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


    createToolbar :: MVar (String, FilePath) -> IO Toolbar
    createToolbar loadNotifier = do
        toolbar <- toolbarNew
        toolbarSetStyle toolbar ToolbarIcons
        openButton <- toolButtonNewFromStock "gtk-open"
        void $ onToolButtonClicked openButton $ do
            openDialog <- createOpenDialog
            dialogResponse' <- dialogRun openDialog
            when (dialogResponse' == ResponseAccept) $ do
                response' <- runMaybeT $ do
                    filepath <- MaybeT $ fileChooserGetFilename openDialog
                    fileFilter <- MaybeT $  fileChooserGetFilter openDialog
                    format <- lift $ fileFilterGetName fileFilter 
                    return (format, filepath)
                when (isJust response') $ putMVar loadNotifier (fromJust response')
            widgetDestroy openDialog

        toolbarInsert toolbar openButton 0

        return toolbar


    createInterface :: MVar (String, FilePath) -> IO (Window, WebView)
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
