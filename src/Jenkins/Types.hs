module Jenkins.Types
  ( Job(..)
  , JobList(..)
  , JobStatus(..)
  , RawBuild(..)
  , JobWithBuildNums(..)
  , JobWithBuilds(..)
  , Branch(..)
  , BuildNum(..)
  , Build(..)
  , BuildParams(..)
  , BuildRev(..)
  , Action(..)
  ) where

import Control.Applicative

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.DateTime as DT
import Data.Monoid ((<>))

import Jenkins.Render

data JobStatus = JobSuccess
               | JobFailure
               | JobInProgress
               | JobAborted
               | JobUnknown
               deriving (Show, Eq)

instance Render JobStatus where
  renderTTY s =
    let (glyph, mColor) = case s of
                            JobSuccess    -> ("✓", Just "0;32")
                            JobFailure    -> ("⨉", Just "0;31")
                            JobInProgress -> ("◷", Just "1;33")
                            JobAborted    -> ("☐", Nothing)
                            JobUnknown    -> ("?", Nothing)
        colorize code   = "\x1b[0" <> code <> "m" <> glyph <> "\x1b[00m"
    in maybe glyph colorize mColor

  render JobSuccess    = "success"
  render JobFailure    = "failure"
  render JobInProgress = "in progress"
  render JobAborted    = "aborted"
  render JobUnknown    = "unknown"

decodeJobStatus :: T.Text -> JobStatus
decodeJobStatus s =
  case s of
    "blue"       -> JobSuccess
    "SUCCESS"    -> JobSuccess
    "red"        -> JobFailure
    "FAILURE"    -> JobFailure
    "grey"       -> JobInProgress
    "blue_anime" -> JobInProgress
    "red_anime"  -> JobInProgress
    "aborted"    -> JobAborted
    _            -> JobUnknown

-------------------------------------------------------------------------------

data Job = Job
         { jobName   :: T.Text
         , jobStatus :: JobStatus
         } deriving (Show, Eq)

instance FromJSON Job where
  parseJSON (Object v) =
    Job                                    <$>
      v .: "name"                          <*>
      liftM decodeJobStatus (v .: "color")
  parseJSON _ = fail "Cannot parse Job"

instance Render Job where
  renderTTY j =
    T.unwords [ renderTTY $ jobStatus j
              , jobName j
              ]
  render j = joinTxt [jobName j, render (jobStatus j)]

-------------------------------------------------------------------------------

newtype JobList = JobList { fromJobList :: [Job] }
                  deriving (Show)
instance FromJSON JobList where
  parseJSON (Object v) = JobList <$> v .: "jobs"
  parseJSON _          = fail "Cannot parse JobList"

instance Render JobList where
  renderTTY = T.unlines . map renderTTY . fromJobList
  render    = T.unlines . map render . fromJobList

-------------------------------------------------------------------------------

data JobWithBuildNums = JobWithBuildNums Job [BuildNum] deriving (Show, Eq)

instance FromJSON JobWithBuildNums where
  parseJSON o@(Object v) = JobWithBuildNums <$>
    parseJSON o <*>
    v .: "builds"
  parseJSON _            = fail "Cannot parse JobWithBuildNums"

-------------------------------------------------------------------------------

data JobWithBuilds = JobWithBuilds Job [Build] deriving (Show, Eq)

instance Render JobWithBuilds where
  renderTTY (JobWithBuilds _ builds) = T.unlines $ map renderTTY builds
  render (JobWithBuilds _ builds)    = T.unlines $ map render builds

-------------------------------------------------------------------------------

type SHA = T.Text
newtype Branch = Branch T.Text deriving (Show, Eq)

instance FromJSON Branch where
  parseJSON (Object v) = Branch <$> v .: "name"
  parseJSON _ = fail "Cannot parse Branch"

instance Render Branch where
  renderTTY (Branch b) = padR 25 $ dropPrefix b
  render (Branch b) = dropPrefix b

dropPrefix :: T.Text -> T.Text
dropPrefix b =
  case T.breakOn "/" b of
    (_, "") -> b
    (_, b') -> T.drop 1 b'

-------------------------------------------------------------------------------

newtype BuildParams = BuildParams {
  fromBuildParams :: [(BS.ByteString, BS.ByteString)]
} deriving (Show, Eq)

instance FromJSON BuildParams where
  parseJSON (Object v) = return . BuildParams $ HM.foldlWithKey' collect [] v
  parseJSON _          = fail "Cannot parse BuildParams"

collect :: [(BS.ByteString, BS.ByteString)]
              -> T.Text
              -> Value
              -> [(BS.ByteString, BS.ByteString)]
collect acc k (String v) = acc ++ [(T.encodeUtf8 k, T.encodeUtf8 v)]
collect _   _ _          = []

-------------------------------------------------------------------------------

data Action = LastBuiltRev SHA [Branch]
            | Params BuildParams
            | OtherAction
            deriving (Show, Eq)

instance FromJSON Action where
  parseJSON (Object v) =
    case (hasKey v lbrKey, hasKey v bpKey) of
      (False, True) -> do
        v' <- v .: bpKey
        Params <$> parseJSON v'
      (True, False) -> do
        v' <- v .: lbrKey
        LastBuiltRev <$> v' .: "SHA1" <*> v' .: "branch"
      _          -> return OtherAction
  parseJSON _ = fail "Cannot parse LastBuiltRevision"

lbrKey :: T.Text
lbrKey = "lastBuiltRevision"

bpKey :: T.Text
bpKey = "parameters"

hasKey :: Object -> T.Text -> Bool
hasKey o k = case HM.lookup k o of
               Just (Object _) -> True
               _               -> False

-------------------------------------------------------------------------------

newtype BuildNum = BuildNum { fromBuildNum :: Int } deriving (Show, Eq)

instance FromJSON BuildNum where
  parseJSON (Object v) = BuildNum <$> v .: "number"
  parseJSON _          = fail "Cannot parse BuildNum"

instance Render BuildNum where
  render (BuildNum n)= "# " <> padR 3 (T.pack (show n))

-------------------------------------------------------------------------------

data RawBuild = RawBuild
              { rawBuildNumber    :: BuildNum
              , rawBuildResult    :: JobStatus
              , rawBuildTimestamp :: Integer
              , rawBuildDuration  :: Integer
              , rawBuildActions   :: [Action]
              } deriving (Show, Eq)

instance FromJSON RawBuild where
  parseJSON (Object v) = do
    res <- v .: "result"
    RawBuild                <$>
      parseJSON (Object v)  <*>
      result res            <*>
      v .: "timestamp"      <*>
      v .: "duration"       <*>
      v .: "actions"
  parseJSON _  = fail "Cannot parse RawBuild"

result :: Value -> Parser JobStatus
result Null       = return JobInProgress
result (String s) = return $ decodeJobStatus s
result _          = return JobUnknown

-------------------------------------------------------------------------------

newtype BuildRev = BuildRev (SHA, Branch) deriving (Show, Eq)

instance Render BuildRev where
  renderTTY (BuildRev (sha, branch)) = T.unwords [ T.take 10 sha
                                                 , renderTTY branch
                                                 ]
  render (BuildRev (sha, branch))    = joinTxt [sha, render branch]

data Build = Build
           { buildNumber    :: BuildNum
           , buildResult    :: JobStatus
           , buildTimestamp :: Integer
           , buildDuration  :: Integer
           , buildParams    :: BuildParams
           , buildRevision  :: Maybe BuildRev
           } deriving (Show, Eq)

instance Render Build where
  renderTTY b = T.unwords $ [ renderTTY (buildResult b)
                            , renderTTY (buildNumber b)
                            , showDateTime . buildTimestamp $ b
                            , showDuration . buildDuration $ b
                            ] <> maybe [] (\br -> [renderTTY br]) (buildRevision b)

  render b = joinTxt $ [ render (buildResult b)
                       , render (buildNumber b)
                       , showDateTime . buildTimestamp $ b
                       , showDuration . buildDuration $ b
                       ] <> maybe [] (\br -> [render br]) (buildRevision b)

showDateTime :: Integer -> T.Text
showDateTime timestamp =
  let d = DT.fromSeconds (timestamp `div` 1000)
  in T.pack $ DT.formatDateTime "%m-%d-%y %H:%M" d

showDuration :: Integer -> T.Text
showDuration 0 = padL 7 "n/a"
showDuration d =
  let d'   = d  `div` 1000
      mins = d' `div` 60
      secs = d' `mod` 60
  in T.unwords [ (padL 2 . T.pack . show $ mins) <> "m"
               , (padL 2 . T.pack . show $ secs) <> "s"
               ]

--------------------------------------------------------------------------------

data BuildParamDef = StringParam T.Text T.Text (Maybe T.Text)
                   | TextParam T.Text T.Text (Maybe T.Text)
                   | PasswordParam T.Text T.Text (Maybe T.Text)
                   | ChoiceParam T.Text T.Text [T.Text] (Maybe T.Text)
                   | BoolParam T.Text T.Text (Maybe T.Text)
                   | RunParam T.Text T.Text (Maybe T.Text)
                   | FileParam T.Text (Maybe T.Text)
                   | UnknownParam T.Text
                   deriving (Show)

instance FromJSON BuildParamDef where
  parseJSON (Object v) = do
    paramType <- v .: "type"
    case paramType of
      (String "StringParameterDefinition")   -> parseParam StringParam v
      (String "TextParameterDefinition")     -> parseParam TextParam v
      (String "PasswordParameterDefinition") -> parseParam PasswordParam v
      (String "BooleanParameterDefinition")  -> parseParam BoolParam v
      (String "ChoiceParameterDefinition")   -> ChoiceParam <$> v .: "name" <*> defaultValue v <*> v .: "choice" <*> v .: "description"
      (String "RunParameterDefinition")      -> RunParam    <$> v .: "name" <*> v .: "project" <*> v .: "description"
      (String "FileParameterDefinition")     -> FileParam   <$> v .: "name" <*> v .: "description"
      (String t)                             -> return (UnknownParam t)

parseParam :: (T.Text -> T.Text -> Maybe T.Text -> BuildParamDef)
           -> Object
           -> Parser BuildParamDef
parseParam f o =  f <$> o .: "name"
                    <*> defaultValue o
                    <*> o .: "description"

defaultValue :: Object -> Parser T.Text
defaultValue o = do
  v <- o .: "defaultValue"
  v .: "value"
