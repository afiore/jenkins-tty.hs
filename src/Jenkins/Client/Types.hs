{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jenkins.Client.Types
  ( Env(..)
  , Client(..)
  , option
  , manager
  , withResponseBody
  , handlingFailures
  ) where


import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as LBS

import Control.Monad.Trans
import Control.Monad.Reader

import Network.HTTP.Client
import Options

data Env = Env
         { envOpts    :: Options
         , envManager :: Manager
         }

newtype Client a = Client {
  runClient :: ReaderT Env IO a
} deriving (Monad, MonadIO, MonadReader Env)


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
