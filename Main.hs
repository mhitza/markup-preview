module Main where

    import Application.GUI
    import Application.CommandLine
    import Application.FileHandling

    import Graphics.UI.Gtk.WebKit.WebView (WebViewClass)
    import Data.Time.Clock (UTCTime(..))

    -- logic related imports
    import Control.Monad (void, when)
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.MVar

    -- transformation related imports
    import System.Directory (getModificationTime)


    loaderReloader :: WebViewClass self
                   => self
                   -> MVar (String, FilePath)
                   -> Maybe (String, FilePath)
                   -> Maybe UTCTime
                   -> IO b
    loaderReloader webView loadNotifier Nothing Nothing = do
        threadDelay 500
        (format, filepath) <- takeMVar loadNotifier
        modificationTime <- getModificationTime filepath
        renderHtml (format, filepath) >>= loadFile webView
        loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime)

    loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime) = do
        threadDelay 500
        noNewFile <- isEmptyMVar loadNotifier
        if noNewFile
            then do modificationTime' <- getModificationTime filepath
                    when (modificationTime' /= modificationTime) $ do
                        renderHtml (format, filepath) >>= loadFile webView
                    loaderReloader webView loadNotifier (Just (format, filepath)) (Just modificationTime')
            else loaderReloader webView loadNotifier Nothing Nothing


    main :: IO ()
    main = withCommandLine $ \initialLoad ->
        withGUI $ do
            loadNotifier <- case initialLoad of Just x  -> newMVar x
                                                Nothing -> newEmptyMVar
            (window, webView) <- createInterface loadNotifier
            void $ forkIO $ loaderReloader webView loadNotifier Nothing Nothing
            return window
