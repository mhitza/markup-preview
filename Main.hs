module Main where

    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

    import Control.Monad (void)


    createInterface = do
        window <- G.windowNew
        scrolledWindow <- G.scrolledWindowNew Nothing Nothing
        webView <- GW.webViewNew
        G.set window [ G.containerChild := scrolledWindow
                     , G.windowDefaultWidth := 600
                     , G.windowDefaultHeight := 600
                     , G.containerBorderWidth := 2
                     ]
        G.set scrolledWindow [ G.containerChild := webView ]
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
