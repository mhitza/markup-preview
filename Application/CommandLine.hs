module Application.CommandLine (withCommandLine) where

    import System.Console.CmdArgs.Explicit

    import Control.Applicative ((<$>))
    import Data.List (isSuffixOf, find)


    arguments :: Mode [(String,String)]
    arguments = mode "markup-preview" [] "" (flagArg (upd "file") "file")
        [ flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        ]
        where upd msg x v = Right $ (msg,x):v


    getArgumentFile :: FilePath -> Maybe (String, FilePath)
    getArgumentFile filepath = flip (,) filepath . fst <$> find (any (`isSuffixOf` filepath) . snd)
                                    [ ("Markdown", [".markdown", ".md"])
                                    , ("Textile", [".textile"])
                                    , ("reStructuredText", [".rst", ".rest", ".restx"]) ]


    withCommandLine :: (Maybe (String, FilePath) -> IO ()) -> IO ()
    withCommandLine f = do
        args <- processArgs arguments
        let hasFlag flag = (flag, "") `elem` args
        let initialLoad = lookup "file" args >>= getArgumentFile
        if hasFlag "help"
            then print $ helpText [] HelpFormatDefault arguments
            else f initialLoad
