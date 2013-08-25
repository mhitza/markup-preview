{-# LANGUAGE CPP #-}
module Application.FileHandling (detectFiletype, renderHtml) where

    import Application.Types

    import Text.Pandoc
    import GHC.IO.Handle
    import System.IO.Temp

#ifdef CABAL
    import Paths_markup_preview
#endif

    import Control.Applicative
    import Data.List
    import Data.List.Utils
    import Data.Maybe


    detectFiletype :: FilePath -> Maybe FileType
    detectFiletype filepath = fst <$> find (any (`isSuffixOf` filepath) . snd)
                            [ (Markdown, [".markdown", ".md"])
                            , (Textile, [".textile"])
                            , (ReStructuredText, [".rst", ".rest", ".restx"]) ]


    readResource :: FilePath -> IO String
    readResource filepath =
#ifdef CABAL
        getDataFileName filepath >>= \filepath' -> readFile filepath'
#endif
#ifndef CABAL
        readFile filepath
#endif


    renderHtml :: (FileType, FilePath) -> IO String
    renderHtml (format, filepath) = readFile filepath >>= writeHtmlFile where
        readerF = fromJust $ lookup format [(Markdown, readMarkdown), (ReStructuredText, readRST), (Textile, readTextile)]
        writer = writeHtmlString def
        reader = readerF (def { readerStandalone = True })
        writeHtmlFile content = do
            -- make this cross OS path style handling
            let tempDirectory = (++ "/") . join "/" . init . split "/" $ filepath
            layout <- readResource "Resources/layout.html"
            let htmlContent = renderTemplate [("htmlContent", writer $ reader content)] layout
            (tempFilePath, tempHandle) <- openTempFile tempDirectory "markup-preview.html"
            hPutStr tempHandle htmlContent >> hFlush tempHandle
            return ("file://" ++ tempFilePath)
