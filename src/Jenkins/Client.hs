{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jenkins.Client
  ( jobStatuses
  , jobStatus
  , jobBuild
  , runBuild
  , buildLog
  , Client(..)
  , Env(..)
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString as BS

import Data.Aeson
import Data.Maybe (catMaybes)

import Control.Monad.Trans
import Control.Monad.Reader

import Control.Concurrent.Async

import Network.HTTP.Client

import Options
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

data Env = Env
         { envOpts    :: Options
         , envManager :: Manager
         }

newtype Client a = Client {
  runClient :: ReaderT Env IO a
} deriving (Monad, MonadIO, MonadReader Env)

jobStatuses :: Client [Job]
jobStatuses = do
  req <- liftIO JEP.getJobs
  handlingFailures req (return . fromJobList)

jobStatus :: T.Text
          -> Client JobWithBuilds
jobStatus name = do
    env <- ask
    req <- liftIO $ JEP.getJob name

    handlingFailures req $ \(JobWithBuildNums job nums) -> do
      let nums'        = take 10 nums
          runJobBuild' = runJobBuild env
      mBuilds <- liftIO $ mapConcurrently runJobBuild' nums'
      let builds = catMaybes mBuilds
      return $ JobWithBuilds job builds
  where
    runJobBuild :: Env -> BuildNum -> IO (Maybe Build)
    runJobBuild e bn = do
      runReaderT (runClient (jobBuild name bn)) e

jobBuild :: T.Text
         -> BuildNum
         -> Client (Maybe Build)
jobBuild name n = do
    req <- liftIO $ JEP.getBuild name n
    handlingFailures req (return . buildWithRev)

runBuild :: T.Text
         -> Maybe T.Text  -- ^ Git revision
         -> Client ()
runBuild name mRev = do
    req <- liftIO $ JEP.runBuild name mRev
    withResponseBody req (return . const ())

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

-------------------------------------------------------------------------------

buildWithRev :: RawBuild -> Maybe Build
buildWithRev (RawBuild n r t d as) =
  fmap (Build n r t d) (findLastBuiltRev as)

findLastBuiltRev :: [Action] -> Maybe BuildRev
findLastBuiltRev ((LastBuiltRev sha (branch:_)):_) = Just (sha, branch)
findLastBuiltRev (_:rest) = findLastBuiltRev rest
findLastBuiltRev [] = Nothing

-------------------------------------------------------------------------------

option :: (Options -> a) -> Client a
option f = do
  env <- ask
  return $ f (envOpts env)

manager :: Client Manager
manager = do
  env <- ask
  return $ envManager env

withResponseBody ::  Request
                 -> (LBS.ByteString -> IO b)
                 -> Client b
withResponseBody req f = do
  m    <- manager
  resp <- liftIO $ httpLbs req m
  liftIO $ f (responseBody resp)

handlingFailures :: FromJSON a 
                 => Request
                 -> (a -> IO b)
                 -> Client b
handlingFailures req f = do
  withResponseBody req $ \body -> do
    failingOnLeft (eitherDecode body) f

failingOnLeft :: FromJSON a
              => Either String a
              -> (a -> IO b)
              -> IO b
failingOnLeft (Right v)     f = f v
failingOnLeft (Left errMsg) _ =fail $ "Failed parsing JSON " ++ errMsg
