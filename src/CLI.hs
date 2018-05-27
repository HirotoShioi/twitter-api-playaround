module CLI
    ( getCliArgs
    , CLI(..)
    ) where

import           RIO

import           Options.Applicative (Parser, ParserInfo, argument, command,
                                      execParser, fullDesc, header, help,
                                      helper, hsubparser, info, infoOption,
                                      long, metavar, progDesc, str, (<**>))
import           Paths_school_aeson  (version)

data CLI = SearchTweets String
         | CountTweets String
         | UserTimeline
         deriving (Show)

-- | Parser for ProcessTicket
cmdSearchTweets :: Parser CLI
cmdSearchTweets = SearchTweets <$> argument str
                     (metavar "Hashtag"
                     <> help "Hashtag to look up")

cmdCountTweets :: Parser CLI
cmdCountTweets = CountTweets <$> argument str
                    (metavar "Hashtag"
                    <> help "Hashtag to look up")

-- | Parser for CLI commands
cli :: Parser CLI
cli = hsubparser $ mconcat
    [ command "search-tweets" (info cmdSearchTweets
        (progDesc "Look up tweets with given hashtag"))
    , command "count-tweets" (info cmdCountTweets
        (progDesc "Count tweets with given hashtag"))
    , command "user-timeline" (info (pure UserTimeline)
        (progDesc "Fetch user's timeline"))
    ]

-- | Get CLI arguments from command line
getCliArgs :: IO CLI
getCliArgs = execParser opts
  where
    opts ::  ParserInfo CLI
    opts = info (cli <**> helper <**> versionHelper)
        ( fullDesc
        <> header "Twitter api client"
        <> progDesc "Twitter api client"
        )
    versionHelper :: Parser (a -> a)
    versionHelper = infoOption
        ("API verion" <> show version)
        (long "version" <> help "Show version")
