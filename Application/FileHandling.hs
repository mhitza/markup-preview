{-# LANGUAGE CPP #-}
module Application.FileHandling (detectFiletype, renderHtml) where

    import Text.Pandoc

#ifdef CABAL
    import Paths_markup_preview
#endif

    import Control.Applicative
    import Data.List
    import Data.Maybe


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


    renderHtml :: (String, FilePath) -> IO (String, FilePath)
    renderHtml (format, filepath) = readFile filepath >>= writeHtmlFile where
        readerF = fromJust $ lookup format [("Markdown", readMarkdown), ("reStructuredText", readRST), ("Textile", readTextile)]
        writer = writeHtmlString def
        reader = readerF (def { readerStandalone = True })
        writeHtmlFile content = do
            layout <- readResource "Resources/layout.html"
            return $ (renderTemplate [("htmlContent", writer $ reader content)] layout, filepath)
