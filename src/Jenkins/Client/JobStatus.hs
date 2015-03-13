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
    env'                        <- env
    req                         <- JEP.getJob name
    (JobWithBuildNums job nums) <- decodingResponse req id
    let nums'        = take 10 nums
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
    req <- JEP.getBuild name n
    decodingResponse req buildWithRev

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d as) =
  fmap (Build n r t d) (findLastBuiltRev as)

findLastBuiltRev :: [Action] -> Maybe BuildRev
findLastBuiltRev ((LastBuiltRev sha (branch:_)):_) = Just (BuildRev (sha, branch))
findLastBuiltRev (_:rest) = findLastBuiltRev rest
findLastBuiltRev [] = Nothing
