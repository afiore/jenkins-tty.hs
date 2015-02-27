module Jenkins.Types
  ( Job(..)
  , JobList(..)
  , JobStatus(..)
  , RawBuild(..)
  , JobWithBuildNums(..)
  , JobWithBuilds(..)
  , BuildNum(..)
  , Build(..)
  , Action(..)
  , BuildRev
  ) where

import Control.Applicative

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad
import Data.Aeson

data JobStatus = JobSuccess
               | JobFailure
               | JobInProgress
               | JobUnknown
               deriving (Show, Eq)

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

newtype JobList = JobList { fromJobList :: [Job] }
                  deriving (Show)
instance FromJSON JobList where
  parseJSON (Object v) = JobList <$> v .: "jobs"
  parseJSON _          = fail "Cannot parse JobList"

data JobWithBuildNums = JobWithBuildNums Job [BuildNum] deriving (Show, Eq)
data JobWithBuilds    = JobWithBuilds Job [Build] deriving (Show, Eq)

instance FromJSON JobWithBuildNums where
  parseJSON o@(Object v) = JobWithBuildNums <$>
    parseJSON o <*>
    v .: "builds"
  parseJSON _            = fail "Cannot parse JobWithBuildNums"

---------------------------------------------------------------------------

type SHA = T.Text
newtype Branch = Branch T.Text deriving (Show, Eq)

instance FromJSON Branch where
  parseJSON (Object v) = Branch <$> v .: "name"
  parseJSON _ = fail "Cannot parse Branch"

data Action = LastBuiltRev SHA [Branch]
            | OtherAction
            deriving (Show, Eq)

instance FromJSON Action where
  parseJSON (Object v) = do
    let k = "lastBuiltRevision"
    if HM.member k v
    then do
      vv <- v  .: k
      LastBuiltRev <$> vv .: "SHA1" <*> vv .: "branch"
    else return OtherAction
  parseJSON _ = fail "Cannot parse LastBuiltRevision"

newtype BuildNum = BuildNum { fromBuildNum :: Int } deriving (Show, Eq)
instance FromJSON BuildNum where
  parseJSON (Object v) = BuildNum <$> v .: "number"
  parseJSON _          = fail "Cannot parse BuildNum"

data RawBuild = RawBuild
              { rawBuildNumber    :: BuildNum
              , rawBuildResult    :: JobStatus
              , rawBuildTimestamp :: Int
              , rawBuildDuration  :: Int
              , rawBuildActions   :: [Action]
              } deriving (Show, Eq)

instance FromJSON RawBuild where
  parseJSON (Object v) =
    RawBuild <$>
      parseJSON (Object v)                  <*>
      liftM decodeJobStatus (v .: "result") <*>
      v .: "timestamp"                      <*>
      v .: "duration"                       <*>
      v .: "actions"
  parseJSON _         = fail "Cannot parse RawBuild"

---------------------------------------------------------------------------

type BuildRev = (SHA, Branch)
data Build = Build
           { buildNumber    :: BuildNum
           , buildResult    :: JobStatus
           , buildTimestamp :: Int
           , buildDuration  :: Int
           , buildRevision  :: BuildRev
           } deriving (Show, Eq)
