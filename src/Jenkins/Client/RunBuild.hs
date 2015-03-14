module Jenkins.Client.RunBuild where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Jenkins.Client.Types
import qualified Jenkins.Endpoints as JEP
import Options (BuildParams(..))

runBuild :: T.Text
         -> BuildParams
         -> Client ()
runBuild name params =
  JEP.runBuild name params >>= getResponseBody >> return ()
