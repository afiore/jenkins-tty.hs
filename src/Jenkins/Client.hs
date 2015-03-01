module Jenkins.Client
  ( handleCmd
  ) where

import Control.Monad.Trans (liftIO)
import Options

import Jenkins.Client.Types
import Jenkins.Render

import qualified Jenkins.Client.JobStatuses as Cmd
import qualified Jenkins.Client.JobStatus   as Cmd
import qualified Jenkins.Client.RunBuild    as Cmd
import qualified Jenkins.Client.BuildLog    as Cmd

handleCmd :: Client ()
handleCmd = do
  cmd <- option optsCommand
  case cmd of
    JobStatuses          -> Cmd.jobStatuses        >>= renderM
    (JobStatus jobId)    -> Cmd.jobStatus jobId    >>= renderM
    (RunBuild jobId rev) -> Cmd.runBuild jobId rev >>  liftIO (putStrLn "OK")
    (BuildLog jobId mBn) -> Cmd.buildLog jobId mBn

renderM :: (Show a, Render a) => a -> Client ()
renderM = liftIO . prettyPrint
