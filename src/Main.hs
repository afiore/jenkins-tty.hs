module Main where

import Options.Applicative

import qualified Data.ByteString.Char8 as BS

import Jenkins.Client (handleCmd, showError)
import Jenkins.Client.Types
import Network.HTTP.Client

import System.IO (stderr)
import System.Exit

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
        (Left err) -> BS.hPutStrLn stderr (showError err) >> exitFailure
        (Right _)  -> exitSuccess

  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )
