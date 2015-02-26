module Jenkins.Client.JobStatuses
  ( jobStatuses
  ) where

import Control.Monad.Trans

import Jenkins.Client.Types
import Jenkins.Types
import qualified Jenkins.Endpoints as JEP

jobStatuses :: Client [Job]
jobStatuses = do
  req <- liftIO JEP.getJobs
  handlingFailures req (return . fromJobList)
