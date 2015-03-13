module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.List (null, head)
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
  req                       <- JEP.getJob name
  (JobWithBuildNums _ nums) <- decodingResponse req id
  if (null nums)
  then
    fail "This job has no builds yet"
  else
    putLog name (head nums)

-------------------------------------------------------------------------------

putLog :: T.Text -> BuildNum -> Client ()
putLog job buildNum = do
  req' <- JEP.buildLog job buildNum
  body <- withResponseBody req' id
  liftIO $ LBS.putStrLn body
