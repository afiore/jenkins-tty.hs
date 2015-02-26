module Jenkins.Client.JobStatuses
  ( jobStatuses
  ) where

import Jenkins.Client.Types
import Jenkins.Types
import qualified Jenkins.Endpoints as JEP

jobStatuses :: Client [Job]
jobStatuses = do
  req <- JEP.getJobs
  handlingFailures req (return . fromJobList)
