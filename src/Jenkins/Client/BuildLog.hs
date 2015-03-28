module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text    as T
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
  (JobWithBuildNums _ nums) <- JEP.getJob name >>= decodeResponse
  if null nums
  then
    fail "This job has no builds yet"
  else
    putLog name (head nums)

-------------------------------------------------------------------------------

putLog :: T.Text -> BuildNum -> Client ()
putLog job buildNum = do
  body <- JEP.buildLog job buildNum >>= getResponseBody
  liftIO $ LBS.putStrLn body
