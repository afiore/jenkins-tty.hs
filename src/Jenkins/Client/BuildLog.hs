module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as LBS

import Control.Monad.Reader

import Jenkins.Client.Types
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

buildLog :: T.Text
         -> Maybe BuildNum
         -> Client ()
buildLog name (Just bn) = putLog name bn
buildLog name Nothing   = do
  env <- ask
  req <- JEP.getJob name
  handlingFailures req $ \(JobWithBuildNums _ nums) -> do
    case nums of
      []     -> T.putStrLn "This job has no builds yet."
      (bn:_) -> runClient env $ putLog name bn

-------------------------------------------------------------------------------

putLog :: T.Text -> BuildNum -> Client ()
putLog job buildNum = do
  req' <- JEP.buildLog job buildNum
  withResponseBody req' LBS.putStrLn
