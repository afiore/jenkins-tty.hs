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
  , BuildRev(..)
  , Action(..)
  ) where

import Control.Applicative

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.DateTime as DT
import Data.Monoid ((<>))

import Jenkins.Render

------------------------------------------------------------------------------------------------------------------------

data JobStatus = JobSuccess
               | JobFailure
               | JobInProgress
               | JobUnknown
               deriving (Show, Eq)

instance Render JobStatus where
  render JobSuccess    = "✓"
  render JobFailure    = "⨉"
  render JobInProgress = "◷"
  render JobUnknown    = "?"

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
    _            -> JobUnknown

------------------------------------------------------------------------------------------------------------------------

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
  render j = joinTxt [jobName j, render (jobStatus j)]

------------------------------------------------------------------------------------------------------------------------

newtype JobList = JobList { fromJobList :: [Job] }
                  deriving (Show)
instance FromJSON JobList where
  parseJSON (Object v) = JobList <$> v .: "jobs"
  parseJSON _          = fail "Cannot parse JobList"

instance Render JobList where
  render = T.unlines . (map render) . fromJobList

------------------------------------------------------------------------------------------------------------------------

data JobWithBuildNums = JobWithBuildNums Job [BuildNum] deriving (Show, Eq)

instance FromJSON JobWithBuildNums where
  parseJSON o@(Object v) = JobWithBuildNums <$>
    parseJSON o <*>
    v .: "builds"
  parseJSON _            = fail "Cannot parse JobWithBuildNums"

------------------------------------------------------------------------------------------------------------------------

data JobWithBuilds = JobWithBuilds Job [Build] deriving (Show, Eq)

instance Render JobWithBuilds where
  render (JobWithBuilds _ builds) = T.unlines $ map render builds

------------------------------------------------------------------------------------------------------------------------

type SHA = T.Text
newtype Branch = Branch T.Text deriving (Show, Eq)

instance FromJSON Branch where
  parseJSON (Object v) = Branch <$> v .: "name"
  parseJSON _ = fail "Cannot parse Branch"

instance Render Branch where
  render (Branch b) = case T.breakOn "/" b of
                        (_, "") -> b
                        (_, b') -> T.drop 1 b'

------------------------------------------------------------------------------------------------------------------------

data Action = LastBuiltRev SHA [Branch]
            | OtherAction
            deriving (Show, Eq)

instance FromJSON Action where
  parseJSON (Object v) = do
    if lastBuiltRevPresent v
    then do
      vv <- v .: lbrKey
      LastBuiltRev <$> vv .: "SHA1" <*> vv .: "branch"
    else return OtherAction
  parseJSON _ = fail "Cannot parse LastBuiltRevision"

lbrKey :: T.Text
lbrKey = "lastBuiltRevision"

lastBuiltRevPresent :: Object -> Bool
lastBuiltRevPresent v =
  case HM.lookup lbrKey v of
    Just (Object _) -> True
    _               -> False

------------------------------------------------------------------------------------------------------------------------

newtype BuildNum = BuildNum { fromBuildNum :: Int } deriving (Show, Eq)

instance FromJSON BuildNum where
  parseJSON (Object v) = BuildNum <$> v .: "number"
  parseJSON _          = fail "Cannot parse BuildNum"

instance Render BuildNum where
  render (BuildNum n)= "# " <> T.pack (show n)

------------------------------------------------------------------------------------------------------------------------

data RawBuild = RawBuild
              { rawBuildNumber    :: BuildNum
              , rawBuildResult    :: JobStatus
              , rawBuildTimestamp :: Integer
              , rawBuildDuration  :: Integer
              , rawBuildActions   :: [Action]
              } deriving (Show, Eq)

instance FromJSON RawBuild where
  parseJSON (Object v) = do
    res <- (v .: "result")
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

------------------------------------------------------------------------------------------------------------------------

newtype BuildRev = BuildRev (SHA, Branch) deriving (Show, Eq)

instance Render BuildRev where
  render (BuildRev (sha, branch)) = joinTxt [sha, render branch]

data Build = Build
           { buildNumber    :: BuildNum
           , buildResult    :: JobStatus
           , buildTimestamp :: Integer
           , buildDuration  :: Integer
           , buildRevision  :: BuildRev
           } deriving (Show, Eq)

instance Render Build where
  render b = joinTxt [ render (buildNumber b)
                     , render (buildResult b)
                     , showDateTime . buildTimestamp $ b
                     , showDuration . buildDuration $ b
                     , render (buildRevision b)
                     ]

showDateTime :: Integer -> T.Text
showDateTime timestamp =
  let d = DT.fromSeconds (timestamp `div` 1000)
  in T.pack $ DT.formatDateTime "%m-%d-%y %H:%M" d

showDuration :: Integer -> T.Text
showDuration d =
  let d'   = d  `div` 1000
      mins = d' `div` 60
      secs = d' `mod` 60
  in (T.pack . show $ mins) <> "m" <> " " <>
     (T.pack . show $ secs) <> "s"
