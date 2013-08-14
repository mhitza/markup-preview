module Main where

    import Application.Interface

    -- logic related imports
    import Control.Monad (void, forever, when)
    import System.Posix.Process (forkProcess)
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.MVar
    import Data.Maybe (isJust, fromJust)

    -- transformation related imports
    import Text.Pandoc
    import System.Directory (getTemporaryDirectory, getModificationTime)

    import Debug.Trace


    generateHtml format contents = writeHtmlString def intermediaryDocument where
        intermediaryDocument = 
            let reader = fromJust $ lookup format [("Markdown", readMarkdown), ("reStructuredText", readRST), ("Textile", readTextile)]
            in reader (def { readerStandalone = True }) contents


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
    main = void $ forkProcess $ withGUI $ do
        loadNotifier <- newEmptyMVar :: IO (MVar (String, String))
        (window, webView) <- createInterface loadNotifier
        forkIO $ loaderReloader webView loadNotifier Nothing Nothing
        return window
