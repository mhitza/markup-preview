module Main where

    import Application.GUI
    import Application.CommandLine
    import Application.FileHandling

    -- logic related imports
    import Control.Monad
    import Control.Concurrent
    import Data.Maybe

    -- transformation related imports
    import System.Directory


    loop :: Monad m => a -> (a -> m a) -> m a
    loop a f = f a >>= \a' -> loop a' f


    main :: IO ()
    main = withCommandLine $ \initialLoad ->
        withGUI $ do
            loadNotifier <- case initialLoad of Just x  -> newMVar x
                                                Nothing -> newEmptyMVar
            (window, webView) <- createInterface loadNotifier
            void . forkIO . void $ loop (Nothing, Nothing) $ \(resource, modificationTime) -> do
                threadDelay 500
                if isNothing resource || isNothing modificationTime
                    then do resource' <- takeMVar loadNotifier
                            modificationTime' <- getModificationTime (snd resource')
                            renderHtml resource' >>= loadFile webView
                            return (Just resource', Just modificationTime')
                    else do noNewFile <- isEmptyMVar loadNotifier
                            if noNewFile
                                then do modificationTime' <- getModificationTime (snd $ fromJust resource)
                                        when (modificationTime' /= (fromJust modificationTime)) $ renderHtml (fromJust resource) >>= loadFile webView
                                        return (resource, Just modificationTime')
                                else return (Nothing, Nothing)
            return window
