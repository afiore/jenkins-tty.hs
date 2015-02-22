module Jenkins.Client
  ( jobStatuses
  , jobStatus
  , jobBuild
  , runBuild
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as BS

import Data.Aeson
import Data.Maybe (catMaybes)

import Control.Concurrent.Async

import Network.HTTP.Client
import Network.HTTP.Types (statusCode)

import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

jobStatuses :: Manager -> IO [Job]
jobStatuses m = do
  req <- JEP.getJobs
  handlingFailures m req (return . fromJobList)

jobStatus :: Manager
          -> T.Text
          -> IO JobWithBuilds
jobStatus m name = do
  req <- JEP.getJob name
  handlingFailures m req $ \(JobWithBuildNums job nums) -> do
    let nums' = take 10 nums
    mBuilds <- mapConcurrently (jobBuild m name) nums'
    let builds = catMaybes mBuilds
    return $ JobWithBuilds job builds

jobBuild :: Manager
         -> T.Text
         -> BuildNum
         -> IO (Maybe Build)
jobBuild m name n = do
    req <- JEP.getBuild name n
    handlingFailures m req (return . buildWithRev)

runBuild :: Manager
         -> T.Text
         -> Maybe T.Text  -- ^ Git revision
         -> IO ()
runBuild m name mRev = do
    req <- JEP.runBuild name mRev
    failingUnlessSuccess m req (return . const ())

-----------------------------------------------------------------------------

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d as) =
  fmap (Build n r t d) (findLastBuiltRev as)

findLastBuiltRev :: [Action] -> Maybe BuildRev
findLastBuiltRev ((LastBuiltRev sha (branch:_)):_) = Just (sha, branch)
findLastBuiltRev (_:rest) = findLastBuiltRev rest
findLastBuiltRev [] = Nothing

-----------------------------------------------------------------------------

failingUnlessSuccess :: Manager
                     -> Request
                     -> (BS.ByteString -> IO b)
                     -> IO b
failingUnlessSuccess m req f = do
  resp <- httpLbs req m
  let code = show . statusCode . responseStatus $ resp
      body = responseBody resp
  if head code /= '2'
  then
    fail ("Jenkins API returned unexpected status code: " ++ code)
  else
    f body

handlingFailures :: FromJSON a => Manager
                 -> Request
                 -> (a -> IO b)
                 -> IO b
handlingFailures m req f = do
  failingUnlessSuccess m req $ \body -> do
    let eData = eitherDecode body
    failingOnLeft eData f

failingOnLeft :: FromJSON a => Either String a
              -> (a -> IO b)
              -> IO b
failingOnLeft (Right v)     f = f v
failingOnLeft (Left errMsg) _ = fail $ "Failed parsing JSON " ++ errMsg
