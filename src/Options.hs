module Options
  ( Command(..)
  , Options(..)
  , parseOptions
  ) where

import Options.Applicative

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import Jenkins.Types

type JobId     = T.Text
type Rev       = Maybe T.Text
type AuthCreds = (BS.ByteString, BS.ByteString)

data Command = JobStatuses
             | JobStatus JobId
             | RunBuild JobId Rev
             | BuildLog JobId (Maybe BuildNum)
             deriving (Show, Eq)

data Options = Options
             { optsBaseUri :: String
             , optsAuth    :: Maybe AuthCreds
             , optsCommand :: Command
             } deriving (Show, Eq)

parserInfo :: Parser Command -> String -> ParserInfo Command
parserInfo cmd desc = info (helper <*> cmd) (progDesc desc)

jobIdParser :: String -> ReadM JobId
jobIdParser = return . T.pack

revParser :: String -> ReadM Rev
revParser = pure . Just . T.pack

authCredsParser :: String -> ReadM (Maybe AuthCreds)
authCredsParser s = do
  return $ case BS.splitWith ((==) ':') (BS.pack s) of
    (user:pass:[]) -> Just (user, pass)
    _           -> Nothing

buildNumParser :: String -> ReadM (Maybe BuildNum)
buildNumParser = pure . Just . BuildNum . read

jobStatusParser :: Parser Command
jobStatusParser = JobStatus
  <$> argument (str >>= jobIdParser) ( metavar "JOB_ID" )

runBuildParser :: Parser Command
runBuildParser = RunBuild
  <$> argument (str >>= jobIdParser) ( metavar "JOB_ID" )
  <*> argument (str >>= revParser) ( metavar "GIT_REV"
                                   <> value Nothing
                                   <> help "Git revision or SHA1"
                                   )

buildLogParser :: Parser Command
buildLogParser = BuildLog
  <$> argument (str >>= jobIdParser) ( metavar "JOB_ID" )
  <*> argument (str >>= buildNumParser) ( metavar "BUILD_NUM"
                                        <> value Nothing
                                        <> help "Build number"
                                        )

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption ( short 's'
                <> metavar "JENKINS_URL"
                <> help "Jenkins base URL"
                )
  <*> option (str >>= authCredsParser ) ( short 'u'
             <> metavar "HTTP_AUTH"
             <> value Nothing
             <> help "http authentication credentials (i.e. user:password)"
             )
  <*> subparser
        (  command "jobs" jobStatusesParserInfo
        <> command "job" jobStatusParserInfo
        <> command "build" runBuildParserInfo
        <> command "log" buildLogParserInfo
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

buildLogParserInfo :: ParserInfo Command
buildLogParserInfo =
  parserInfo buildLogParser "stream build log to standard output"
