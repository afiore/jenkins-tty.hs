module Jenkins.Client.JobStatus
  ( jobStatus
  ) where

import qualified Data.Text as T
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
    env <- ask
    req <- JEP.getJob name

    handlingFailures req $ \(JobWithBuildNums job nums) -> do
      let nums'        = take 10 nums
          runJobBuild' = runJobBuild env
      mBuilds <- liftIO $ mapConcurrently runJobBuild' nums'
      let builds = catMaybes mBuilds
      return $ JobWithBuilds job builds
  where
    runJobBuild :: Env -> BuildNum -> IO (Maybe Build)
    runJobBuild e bn = do
      runClient e (jobBuild name bn)

jobBuild :: T.Text
         -> BuildNum
         -> Client (Maybe Build)
jobBuild name n = do
    req <- JEP.getBuild name n
    handlingFailures req (return . buildWithRev)

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d as) =
  fmap (Build n r t d) (findLastBuiltRev as)

findLastBuiltRev :: [Action] -> Maybe BuildRev
findLastBuiltRev ((LastBuiltRev sha (branch:_)):_) = Just (sha, branch)
findLastBuiltRev (_:rest) = findLastBuiltRev rest
findLastBuiltRev [] = Nothing
