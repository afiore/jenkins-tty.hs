module Main where

import Options.Applicative
import Control.Monad.Reader

import Jenkins.Client (handleCmd)
import Jenkins.Client.Types (Env(..), Client(..))
import Network.HTTP.Client (withManager, defaultManagerSettings)
import Options

main :: IO ()
main = do
    opts <- execParser handleOpts
    withManager defaultManagerSettings $ \m -> do
      let env = Env { envOpts    = opts
                    , envManager = m
                    }
      runReaderT (runClient handleCmd) env
  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )
