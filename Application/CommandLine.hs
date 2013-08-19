{-# LANGUAGE ImplicitParams, RankNTypes #-}
module Application.CommandLine (withCommandLine, StartupOptions(..)) where

    import Application.FileHandling

    import System.Console.CmdArgs.Explicit

    import Data.Maybe
    import Data.List
    import Control.Monad


    type SkipExecution = Bool
    type ArgumentKey = String
    type Arguments = [(ArgumentKey, String)]

    data StartupOptions = StartupOptions
                        { file :: Maybe FilePath
                        , filetype :: Maybe String
                        , canLoad :: Bool }


    arguments :: Mode Arguments
    arguments = mode "markup-preview" [] "" (flagArg (upd "file") "file")
        [ flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        , flagNone ["markdown"] (setKey "force-type" "Markdown") "Treat file as Markdown" 
        , flagNone ["textile"] (setKey "force-type" "Textile") "Treat file as Textile" 
        , flagNone ["rst"] (setKey "force-type" "reStructuredText") "Treat file as reStructuredText" 
        ]
        where upd msg x v = Right $ (msg,x):v
              setKey key v xs = case findIndex (\(k,_) -> k == key) xs of
                                    Just _  -> map (\x@(k,_) -> if key == k then (k,v) else x) xs
                                    Nothing -> (key,v):xs


    hasFlag :: ArgumentKey -> Arguments -> Bool
    hasFlag flag = isJust . findIndex (\(k,_) -> k == flag)


    getFlag :: ArgumentKey -> Arguments -> Maybe String
    getFlag flag = listToMaybe . map snd . filter (\(k,_) -> k == flag)


    handleInformationRequest :: Arguments -> IO SkipExecution
    handleInformationRequest args 
        | hasFlag "version" args = putStrLn "markup-preview 0.1.0.0" >> return True
        | hasFlag "help" args    = print (helpText [] HelpFormatDefault arguments) >> return True
        | otherwise              = return False


    buildOptions :: Arguments -> StartupOptions
    buildOptions args = StartupOptions { file=file_option, filetype=filetype_option, canLoad=can_load_option } where
        file_option = getFlag "file" args
        can_load_option = isJust filetype_option
        filetype_option | hasFlag "force-type" args = getFlag "force-type" args
                        | otherwise                 = file_option >>= detectFiletype


    withCommandLine :: ((?startupOptions :: StartupOptions) => IO ()) -> IO ()
    withCommandLine f = do
        args <- processArgs arguments
        skip <- handleInformationRequest args
        unless skip $ let ?startupOptions = buildOptions args in f
