module Options
  ( Command(..)
  , Options(..)
  , parseOptions
  ) where

import Options.Applicative

import qualified Data.Text as T

import Jenkins.Types

type JobId = T.Text
type Rev   = Maybe T.Text

data Command = JobStatuses
             | JobStatus JobId
             | RunBuild JobId Rev
             deriving (Show, Eq)

data Options = Options
             { optBaseUri :: String
             , optCommand :: Command
             } deriving (Show, Eq)

parserInfo :: Parser Command -> String -> ParserInfo Command
parserInfo cmd desc = info (helper <*> cmd) (progDesc desc)

jobIdParser :: String -> Maybe JobId
jobIdParser = return . T.pack

revParser :: String -> Maybe Rev
revParser = pure . Just . T.pack

jobStatusParser :: Parser Command
jobStatusParser = JobStatus
  <$> argument jobIdParser ( metavar "JOB_ID" )

runBuildParser :: Parser Command
runBuildParser = RunBuild
  <$> argument jobIdParser ( metavar "JOB_ID" )
  <*> argument revParser ( metavar "GIT_REV"
                         <> value Nothing
                         <> help "Git revision or SHA1"
                         )

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption ( short 's'
                <> metavar "JENKINS_URL"
                <> help "Jenkins base URL"
                )
  <*> subparser
        ( command "jobs" jobStatusesParserInfo
        <> command "job" jobStatusParserInfo
        <> command "build" runBuildParserInfo
        )

jobStatusesParserInfo :: ParserInfo Command
jobStatusesParserInfo =
  parserInfo (pure JobStatuses) "display all jobs' status"

jobStatusParserInfo :: ParserInfo Command
jobStatusParserInfo =
  parserInfo jobStatusParser "list recent builds for a given job"

runBuildParserInfo :: ParserInfo Command
runBuildParserInfo =
  parserInfo runBuildParser "build a given job"
