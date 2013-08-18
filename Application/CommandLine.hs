module Application.CommandLine (withCommandLine) where

    import Application.FileHandling

    import System.Console.CmdArgs.Explicit

    import Control.Applicative


    arguments :: Mode [(String,String)]
    arguments = mode "markup-preview" [] "" (flagArg (upd "file") "file")
        [ flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        ]
        where upd msg x v = Right $ (msg,x):v


    getArgumentFile :: FilePath -> Maybe (String, FilePath)
    getArgumentFile filepath = flip (,) filepath <$> detectFiletype filepath


    withCommandLine :: (Maybe (String, FilePath) -> IO ()) -> IO ()
    withCommandLine f = do
        args <- processArgs arguments
        let hasFlag flag = (flag, "") `elem` args
        let initialLoad = lookup "file" args >>= getArgumentFile
        if hasFlag "help"
            then print $ helpText [] HelpFormatDefault arguments
            else f initialLoad
