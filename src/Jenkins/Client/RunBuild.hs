module Jenkins.Client.RunBuild where

import qualified Data.Text as T

import Jenkins.Client.Types
import qualified Jenkins.Endpoints as JEP
import Jenkins.Types (BuildParams(..))

runBuild :: T.Text
         -> BuildParams
         -> Client ()
runBuild name params =
  JEP.runBuild name params >>= getResponseBody >> return ()
