module Jenkins.Client.JobStatuses
  ( jobStatuses
  ) where

import Jenkins.Client.Types
import Jenkins.Types
import qualified Jenkins.Endpoints as JEP

jobStatuses :: Client JobList
jobStatuses = do
  req <- JEP.getJobs
  decodingResponse req id