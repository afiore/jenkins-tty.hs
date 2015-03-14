{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jenkins.Client.Types
  ( Env(..)
  , Client(..)
  , AppError(..)
  , env
  , option
  , manager
  , getResponseBody
  , decodeResponse
  ) where


import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Trans
import Network.HTTP.Client
import Network.HTTP.Types
import Options


data Env = Env
         { envOpts    :: Options
         , envManager :: Manager
         }

data AppError = HttpError Status Request (Maybe AuthCreds)
              | JsonError BS.ByteString
              | ClientError BS.ByteString
              deriving (Show)

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
  fail msg = Client $ \_ -> return $ Left . ClientError $ BS.pack msg

instance Functor Client where
  fmap f bl = Client $ \e -> do
    eV <- runClient bl e
    return $ fmap f eV

instance MonadIO Client where
  liftIO ioAction = Client $ \_ -> do
    v <- ioAction
    return $ Right v

failJson msg = Client $ \_ -> return $ Left . JsonError $ BS.pack msg

failHttp status req = Client $ \e -> do
  let authCreds = optsAuth . envOpts $ e
  return . Left $ HttpError status req authCreds

env :: Client Env
env = Client $ \e -> return (Right e)

option :: (Options -> a) -> Client a
option f = Client $ \e -> return $ Right . f . envOpts $ e

manager :: Client Manager
manager = Client $ \e -> return $ Right .  envManager $ e

getResponseBody :: Request
                 -> Client LBS.ByteString
getResponseBody req = do
  m    <- manager
  resp <- liftIO $ httpLbs req m
  let status = responseStatus resp
  if (isSuccess status)
  then
    return $ responseBody resp
  else
    failHttp status req

isSuccess :: Status -> Bool
isSuccess s =
   case (show . statusCode $ s) of
     '2':_:_ -> True
     _       -> False

decodeResponse :: FromJSON a
               => Request
               -> Client a
decodeResponse req =  do
  body <- getResponseBody req
  case (eitherDecode body) of
    (Left msg) -> failJson msg
    (Right v)  -> return v
