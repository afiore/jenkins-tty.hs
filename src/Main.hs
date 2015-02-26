module Main where

import Options.Applicative

import Jenkins.Client (handleCmd)
import Jenkins.Client.Types
import Network.HTTP.Client
import Options

main :: IO ()
main = do
    opts <- execParser handleOpts
    withManager defaultManagerSettings $ \m -> do
      let env = Env { envOpts    = opts
                    , envManager = m
                    }
      runClient env handleCmd
  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )
