{-# LANGUAGE ImplicitParams #-}
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


    createLoadNotifier :: Maybe (String, FilePath) -> IO (MVar (String, FilePath))
    createLoadNotifier (Just (ft,fp)) = newMVar (ft, fp)
    createLoadNotifier Nothing        = newEmptyMVar


    main :: IO ()
    main = withCommandLine $ withGUI $ do
        loadNotifier <- createLoadNotifier (startupFile ?startupOptions)
        (window, webView) <- createInterface loadNotifier
        let loadInsideView r = renderHtml r >>= handleResource webView
        void . forkIO . void $ loop (Nothing, Nothing) $ \(resource, modificationTime) -> do
            threadDelay 500
            if isNothing resource || isNothing modificationTime
                then do resource' <- takeMVar loadNotifier
                        modificationTime' <- getModificationTime (snd resource')
                        loadInsideView resource'
                        return (Just resource', Just modificationTime')
                else do noNewFile <- isEmptyMVar loadNotifier
                        if noNewFile
                            then do modificationTime' <- getModificationTime (snd $ fromJust resource)
                                    when (modificationTime' /= fromJust modificationTime) $ loadInsideView (fromJust resource)
                                    return (resource, Just modificationTime')
                            else return (Nothing, Nothing)
        return window
