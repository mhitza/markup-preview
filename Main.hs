module Main where

    import Application.GUI
    import Graphics.UI.Gtk.WebKit.WebView (WebViewClass)
    import Data.Time.Clock (UTCTime(..))

    -- command line interface imports
    import System.Console.CmdArgs.Explicit

    -- logic related imports
    import Control.Monad (void, when)
    import Control.Applicative ((<$>))
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.MVar
    import Data.Maybe (fromJust)
    import Data.List (isSuffixOf, find)

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


    arguments :: Mode [(String,String)]
    arguments = mode "markup-preview" [] "" (flagArg (upd "file") "file")
        [ flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        ]
        where upd msg x v = Right $ (msg,x):v


    getArgumentFile :: FilePath -> Maybe (String, FilePath)
    getArgumentFile filepath = flip (,) filepath . fst <$> find (any (`isSuffixOf` filepath) . snd)
                                    [ ("Markdown", [".markdown", ".md"])
                                    , ("Textile", [".textile"])
                                    , ("reStructuredText", [".rst", ".rest", ".restx"]) ]


    main :: IO ()
    main = do
        args <- processArgs arguments
        let hasFlag flag = (flag, "") `elem` args
        let initialLoad = lookup "file" args >>= getArgumentFile
        if hasFlag "help"
            then print $ helpText [] HelpFormatDefault arguments
            else withGUI $ do
                loadNotifier <- case initialLoad of Just x  -> newMVar x
                                                    Nothing -> newEmptyMVar
                (window, webView) <- createInterface loadNotifier
                void $ forkIO $ loaderReloader webView loadNotifier Nothing Nothing
                return window
