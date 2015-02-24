module Jenkins.Client
  ( jobStatuses
  , jobStatus
  , jobBuild
  , runBuild
  , buildLog
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS

import Data.Aeson
import Data.Maybe (catMaybes)

import Control.Concurrent.Async

import Network.HTTP.Client

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
    withResponseBody m req (return . const ())

buildLog :: Manager
         -> T.Text
         -> Maybe BuildNum
         -> IO ()
buildLog m name mBnum = do
    case mBnum of
      Just bn -> stream bn
      Nothing -> do
        req <- JEP.getJob name
        handlingFailures m req $ \(JobWithBuildNums _ nums) -> do
          case nums of
            []    -> putStrLn "This job has no builds yet."
            (bn:_) -> stream bn
  where
    stream :: BuildNum -> IO ()
    stream buildNum = do
      req' <- JEP.buildLog name buildNum
      withResponse req' m consumeStream

    consumeStream :: Response BodyReader -> IO ()
    consumeStream s = do
      chunk <- brRead . responseBody $ s
      if BS.null chunk
      then
        putStrLn "done!"
      else
        BS.putStr chunk >> consumeStream s
-----------------------------------------------------------------------------

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d as) =
  fmap (Build n r t d) (findLastBuiltRev as)

findLastBuiltRev :: [Action] -> Maybe BuildRev
findLastBuiltRev ((LastBuiltRev sha (branch:_)):_) = Just (sha, branch)
findLastBuiltRev (_:rest) = findLastBuiltRev rest
findLastBuiltRev [] = Nothing

-----------------------------------------------------------------------------

withResponseBody :: Manager
                 -> Request
                 -> (LBS.ByteString -> IO b)
                 -> IO b
withResponseBody m req f = do
  resp <- httpLbs req m
  f $ responseBody resp

handlingFailures :: FromJSON a => Manager
                 -> Request
                 -> (a -> IO b)
                 -> IO b
handlingFailures m req f = do
  withResponseBody m req $ \body -> do
    let eData = eitherDecode body
    failingOnLeft eData f

failingOnLeft :: FromJSON a => Either String a
              -> (a -> IO b)
              -> IO b
failingOnLeft (Right v)     f = f v
failingOnLeft (Left errMsg) _ = fail $ "Failed parsing JSON " ++ errMsg
