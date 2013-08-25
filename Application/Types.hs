module Application.Types where

    data FileType = Markdown | Textile | ReStructuredText deriving (Read, Eq)

    data StartupOptions = StartupOptions
                        { startupFile :: Maybe (FileType, FilePath) }
