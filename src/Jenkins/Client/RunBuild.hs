module Jenkins.Client.RunBuild where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Jenkins.Client.Types
import qualified Jenkins.Endpoints as JEP

runBuild :: T.Text
         -> [(BS.ByteString, BS.ByteString)] -- ^ Build parameters
         -> Client ()
runBuild name params = do
    req <- JEP.runBuild name params
    withResponseBody req (const ())
