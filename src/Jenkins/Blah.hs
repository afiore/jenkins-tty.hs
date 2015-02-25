{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jenkins.Blah where

import Control.Monad.Trans
import Control.Monad.Reader

data AppConfig = AppConfig
               { foo :: Int
               , bar :: String
               }

newtype App a = App {
  runApp :: ReaderT AppConfig IO a
} deriving (Monad, MonadIO, MonadReader AppConfig)

foobar :: App String
foobar = do
  cfg <- ask
  let foobar' = show (foo cfg) ++ bar cfg
  return foobar'

reverze :: String -> App String
reverze = return . reverse

doMonadTrans :: IO ()
doMonadTrans = do
  let cfg = AppConfig { foo = 22, bar = "ciao"}
      app = App (return ())
  v <- runReaderT (runApp  (app >> foobar >>= reverze)) cfg
  putStrLn $ "got " ++ v
  return ()
