module Main where

    import Graphics.UI.Gtk (AttrOp((:=)))
    import qualified Graphics.UI.Gtk as G
    import qualified Graphics.UI.Gtk.WebKit.WebView as GW

    import Control.Monad (void)


    createWindowAndView = do
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


    main :: IO ()
    main = do
        void $ G.initGUI
        (window, webView) <- createWindowAndView
        GW.webViewLoadUri webView "http://reddit.com"
        G.onDestroy window G.mainQuit
        G.widgetShowAll window
        G.mainGUI
