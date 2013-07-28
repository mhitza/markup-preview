module Main where

    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

    import Control.Monad (void)


    createInterface = do
        window <- G.windowNew
        scrolledWindow <- G.scrolledWindowNew Nothing Nothing
        webView <- GW.webViewNew
        G.set scrolledWindow [ G.containerChild := webView ]

        singleColumn <- G.vBoxNew False 1
        G.boxPackStart singleColumn scrolledWindow G.PackGrow 0

        G.set window [ G.containerChild := singleColumn
                     , G.windowDefaultWidth := 600
                     , G.windowDefaultHeight := 600
                     , G.containerBorderWidth := 2
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
        (window, webView) <- createInterface
        GW.webViewLoadUri webView "http://reddit.com"
        return window
