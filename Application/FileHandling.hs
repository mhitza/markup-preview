{-# LANGUAGE CPP #-}
module Application.FileHandling where

    import Text.Pandoc (renderTemplate)
    import GHC.IO.Handle (hPutStr, hFlush)
    import System.Directory (getTemporaryDirectory)
    import System.IO.Temp (openTempFile)

    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

#ifdef CABAL
    import Paths_markup_preview
#endif

    import Control.Applicative ((<$>))
    import Data.List (isSuffixOf, find)


    detectFiletype :: FilePath -> Maybe String
    detectFiletype filepath = fst <$> find (any (`isSuffixOf` filepath) . snd)
                            [ ("Markdown", [".markdown", ".md"])
                            , ("Textile", [".textile"])
                            , ("reStructuredText", [".rst", ".rest", ".restx"]) ]


    readResource :: FilePath -> IO String
    readResource filepath =
#ifdef CABAL
        getDataFileName filepath >>= \filepath' -> readFile filepath'
#endif
#ifndef CABAL
        readFile filepath
#endif

    loadHtmlInView webView htmlContent = do
            tempDirectory <- getTemporaryDirectory
            layout <- readResource "Resources/layout.html"
            let htmlContent' = renderTemplate [("htmlContent", htmlContent)] layout
            (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview.html"
            hPutStr tempHandle htmlContent' >> hFlush tempHandle
            GW.webViewLoadUri webView ("file://" ++ tempFilePath)
