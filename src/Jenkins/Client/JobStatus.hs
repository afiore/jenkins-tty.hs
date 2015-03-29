module Jenkins.Client.JobStatus
  ( jobStatus
  ) where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe (catMaybes)

import Control.Concurrent.Async

import Control.Monad.Trans
import Control.Monad.Reader

import Jenkins.Client.Types
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

jobStatus :: T.Text
          -> Client JobWithBuilds
jobStatus name = do
    env'                        <- env
    (JobWithBuildNums job nums) <- JEP.getJob name >>= decodeResponse

    let nums' = take 10 nums
    mBuilds <- liftIO $ mapConcurrently (runJobBuild env') nums'
    let builds = catMaybes mBuilds
    return $ JobWithBuilds job builds
  where
    runJobBuild :: Env -> BuildNum -> IO (Maybe Build)
    runJobBuild e bn = do
      eBuild <- runClient (jobBuild name bn) e
      return $ case eBuild of
                 (Right mBuild) -> mBuild
                 (Left _)       -> Nothing

-------------------------------------------------------------------------------

jobBuild :: T.Text
         -> BuildNum
         -> Client (Maybe Build)
jobBuild name n = do
  rawBuidl <- JEP.getBuild name n >>= decodeResponse
  return $ buildWithRev rawBuidl

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d actions) =
  case parseActions actions of
    (Just bp, mbr@(Just _)) -> Just (Build n r t d bp mbr)
    (Just bp, Nothing)      -> Just (Build n r t d bp Nothing)
    _                       -> Nothing

type MParamsWithBuildRev = (Maybe BuildParams, Maybe BuildRev)

parseActions :: [Action] -> MParamsWithBuildRev
parseActions =
    L.foldl f (Nothing, Nothing)
  where
    f :: MParamsWithBuildRev -> Action -> MParamsWithBuildRev
    f (Nothing, mLbr) (Params bp)              = (Just bp, mLbr)
    f (mPb, Nothing) (LastBuiltRev sha (br:_)) = (mPb, Just (BuildRev (sha, br)))
    f tup            _                         = tup

