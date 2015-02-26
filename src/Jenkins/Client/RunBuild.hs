module Jenkins.Client.RunBuild where

import qualified Data.Text as T

import Control.Monad.Trans

import Jenkins.Client.Types
import qualified Jenkins.Endpoints as JEP

runBuild :: T.Text
         -> Maybe T.Text  -- ^ Git revision
         -> Client ()
runBuild name mRev = do
    req <- liftIO $ JEP.runBuild name mRev
    withResponseBody req (return . const ())
