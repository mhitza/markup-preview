{-# LANGUAGE ImplicitParams, RankNTypes #-}
module Application.CommandLine (withCommandLine, StartupOptions(..)) where

    import Application.FileHandling

    import System.Console.CmdArgs.Explicit

    import Data.Maybe


    type SkipExecution = Bool
    type ArgumentKey = String
    type Arguments = [(ArgumentKey, String)]

    data StartupOptions = StartupOptions
                        { file :: Maybe FilePath
                        , filetype :: Maybe String
                        , can_load :: Bool }


    arguments :: Mode Arguments
    arguments = mode "markup-preview" [] "" (flagArg (upd "file") "file")
        [ flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        ]
        where upd msg x v = Right $ (msg,x):v


    hasFlag :: ArgumentKey -> Arguments -> Bool
    hasFlag flag = elem (flag, "")


    handleInformationRequest :: Arguments -> IO SkipExecution
    handleInformationRequest args 
        | hasFlag "version" args = putStrLn "markup-preview 0.1.0.0" >> return True
        | hasFlag "help" args    = print (helpText [] HelpFormatDefault arguments) >> return True
        | otherwise              = return False


    buildOptions :: Arguments -> StartupOptions
    buildOptions args = StartupOptions { file=file_option, filetype=filetype_option, can_load=can_load_option } where
        file_option = lookup "file" args
        filetype_option = file_option >>= detectFiletype
        can_load_option = isJust filetype_option


    withCommandLine :: ((?startupOptions :: StartupOptions) => IO ()) -> IO ()
    withCommandLine f = do
        args <- processArgs arguments
        skip <- handleInformationRequest args
        if skip
            then return ()
            else let ?startupOptions = buildOptions args in f
