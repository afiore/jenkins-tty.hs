module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad.Reader
import Network.HTTP.Client

import Jenkins.Client.Types
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

buildLog :: T.Text
         -> Maybe BuildNum
         -> Client ()
buildLog name (Just bn) = stream name bn
buildLog name Nothing   = do
  req <- JEP.getJob name
  env <- ask
  handlingFailures req $ \(JobWithBuildNums _ nums) -> do
    case nums of
      []     -> putStrLn "This job has no builds yet."
      (bn:_) -> runClient env $ stream name bn

stream :: T.Text -> BuildNum -> Client ()
stream job buildNum = do
  req' <- JEP.buildLog job buildNum
  m    <- manager
  liftIO $ withResponse req' m consumeStream

consumeStream :: Response BodyReader -> IO ()
consumeStream s = do
  chunk <- brRead . responseBody $ s
  if BS.null chunk
  then
    putStrLn "done!"
  else
    BS.putStr chunk >> consumeStream s
