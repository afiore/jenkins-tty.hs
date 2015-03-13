{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jenkins.Client.Types
  ( Env(..)
  , Client(..)
  , env
  , option
  , manager
  , withResponseBody
  , decodingResponse
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

data AppError = HttpError Int
              | JsonError String
              | ClientError String
              deriving (Show, Eq)


newtype Client a = Client {
  runClient :: Env -> IO (Either AppError a)
}

instance Monad Client where
  return x = Client $ \_ -> return (Right x)
  bl >>= f = Client $ \e -> do
    eV <- runClient bl e
    case eV of
      (Right v)  -> runClient (f v) e
      (Left  err)  -> return $ Left err
  fail msg = Client $ \_ -> return $ Left . ClientError $ msg

instance Functor Client where
  fmap f bl = Client $ \e -> do
    eV <- runClient bl e
    return $ fmap f eV

instance MonadIO Client where
  liftIO ioAction = Client $ \_ -> do
    v <- ioAction
    return $ Right v

failJson msg = Client $ \_ -> return $ Left . JsonError $ msg
failHttp msg = Client $ \_ -> return $ Left . HttpError $ msg

env :: Client Env
env = Client $ \e -> return (Right e)

option :: (Options -> a) -> Client a
option f = Client $ \e -> return $ Right . f . envOpts $ e

manager :: Client Manager
manager = Client $ \e -> return $ Right .  envManager $ e

withResponseBody :: Request
                 -> (LBS.ByteString -> b)
                 -> Client b
withResponseBody req f = do
  m    <- manager
  resp <- liftIO $ httpLbs req m
  return $ f (responseBody resp)

decodingResponse :: FromJSON a
                 => Request
                 -> (a -> b)
                 -> Client b
decodingResponse req f =  do
  body <- withResponseBody req id
  case (eitherDecode body) of
    (Left msg) -> failJson msg
    (Right v)  -> return (f v)
