module Options
  ( Command(..)
  , Options(..)
  , BuildParams(..)
  , AuthCreds(..)
  , parseOptions
  ) where

import Options.Applicative

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)

import Jenkins.Types

type JobId       = T.Text

newtype BuildParams = BuildParams {
  fromBuildParams :: [(BS.ByteString, BS.ByteString)]
} deriving (Show, Eq)

newtype AuthCreds = AuthCreds {
  fromAuthCreds :: (BS.ByteString, BS.ByteString)
} deriving (Show, Eq)

data Command = JobStatuses
             | JobStatus JobId
             | RunBuild JobId BuildParams
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

buildParamParser :: String -> ReadM BuildParams
buildParamParser = return . BuildParams . mapMaybe parseParam . BS.words . BS.pack

parseParam :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseParam s =
  case (BS.break ((==) '=') s) of
      (_, "") -> Nothing
      (k, v)  -> Just (k, BS.drop 1 v)

authCredsParser :: String -> ReadM (Maybe AuthCreds)
authCredsParser s = do
  return $ case BS.splitWith ((==) ':') (BS.pack s) of
    (user:pass:[]) -> Just (AuthCreds (user, pass))
    _              -> Nothing

buildNumParser :: String -> ReadM (Maybe BuildNum)
buildNumParser = pure . Just . BuildNum . read

jobStatusParser :: Parser Command
jobStatusParser = JobStatus
  <$> argument (str >>= jobIdParser) ( metavar "JOB_ID" )

runBuildParser :: Parser Command
runBuildParser = RunBuild
  <$> argument (str >>= jobIdParser) ( metavar "JOB_ID" )
  <*> argument (str >>= buildParamParser) ( metavar "PARAM=VALUE .."
                                   <> value (BuildParams [])
                                   <> help "List of parameter/value pairs"
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
