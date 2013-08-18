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
    import Data.Maybe (fromJust)

    -- transformation related imports
    import Text.Pandoc
    import System.Directory (getModificationTime)


    generateHtml :: String -> String -> String
    generateHtml format contents = writeHtmlString def intermediaryDocument where
        intermediaryDocument = 
            let reader = fromJust $ lookup format [("Markdown", readMarkdown), ("reStructuredText", readRST), ("Textile", readTextile)]
            in reader (def { readerStandalone = True }) contents


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
    main = withCommandLine $ \initialLoad ->
        withGUI $ do
            loadNotifier <- case initialLoad of Just x  -> newMVar x
                                                Nothing -> newEmptyMVar
            (window, webView) <- createInterface loadNotifier
            void $ forkIO $ loaderReloader webView loadNotifier Nothing Nothing
            return window
