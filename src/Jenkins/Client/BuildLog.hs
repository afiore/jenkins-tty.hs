module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad.Trans
import Network.HTTP.Client

import Jenkins.Client.Types
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

buildLog :: T.Text
         -> Maybe BuildNum
         -> Client ()
buildLog name mBnum = do
    m <- manager
    case mBnum of
      Just bn -> liftIO $ stream m bn
      Nothing -> do
        req <- liftIO $ JEP.getJob name
        handlingFailures req $ \(JobWithBuildNums _ nums) -> do
          liftIO $ case nums of
            []     -> putStrLn "This job has no builds yet."
            (bn:_) -> stream m bn
  where
    stream :: Manager -> BuildNum -> IO ()
    stream m buildNum = do
      req' <- JEP.buildLog name buildNum
      withResponse req' m consumeStream

consumeStream :: Response BodyReader -> IO ()
consumeStream s = do
  chunk <- brRead . responseBody $ s
  if BS.null chunk
  then
    putStrLn "done!"
  else
    BS.putStr chunk >> consumeStream s
