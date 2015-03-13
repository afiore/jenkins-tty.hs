module Main where

import Data.Either
import Options.Applicative

import Jenkins.Client (handleCmd)
import Jenkins.Client.Types
import Network.HTTP.Client

import System.Exit
import System.IO

import Options

main :: IO ()
main = do
    opts <- execParser handleOpts
    withManager defaultManagerSettings $ \m -> do
      let e = Env { envOpts    = opts
                  , envManager = m
                  }

      eError <- runClient handleCmd e
      case eError of
        (Left err) -> hPutStrLn stderr (show err) >> exitFailure
        (Right _)  -> exitSuccess

  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )
