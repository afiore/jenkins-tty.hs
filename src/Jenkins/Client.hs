module Jenkins.Client
  ( handleCmd
  , showError
  ) where

import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Trans (liftIO)
import Options

import Network.HTTP.Types
import Network.HTTP.Client

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
    JobStatuses             -> Cmd.jobStatuses           >>= renderM
    (JobStatus jobId)       -> Cmd.jobStatus jobId       >>= renderM
    (RunBuild jobId params) -> Cmd.runBuild jobId params >>  liftIO (putStrLn "OK")
    (BuildLog jobId mBn)    -> Cmd.buildLog jobId mBn

renderM :: Render a => a -> Client ()
renderM = liftIO . prettyPrint

--------------------------------------------------------------------------------

showError :: AppError -> BS.ByteString
showError (JsonError msg)        = "A JSON parsing error occurred: " <> msg
showError (ClientError msg)      = msg
showError (HttpError status req mCreds) =
  let code   = BS.pack . show . statusCode $ status
  in BS.unlines [ "Jenkins API responded with non-success status code " <> code
                , "To reproduce the generated HTTP request, run the following curl command: "
                , ""
                , showCurlCmd req mCreds
                ]

showCurlCmd :: Request -> Maybe AuthCreds -> BS.ByteString
showCurlCmd req auth =
  let method' = if method req == methodPost then Just "-XPOST" else Nothing
      proto   = if secure req then "https://" else "http://"
      port'   = if port req == 80 then "" else ":" <> show (port req)
      url     = Just (proto <> host req <> BS.pack port' <> path req <> queryString req)
  in BS.unwords $ catMaybes [ Just "curl"
                            , method'
                            , fmap authOpt auth
                            , url
                            ]

authOpt :: AuthCreds -> BS.ByteString
authOpt (AuthCreds (user, pass)) = "-u " <> user <> ":" <> pass

--------------------------------------------------------------------------------
