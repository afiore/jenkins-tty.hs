module Main where

import Options.Applicative

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Jenkins.Client as Jen
import Jenkins.Client (Env(..), Client(..))
import qualified Network.HTTP.Client as HTTP
import Jenkins.Blah
import Options

main :: IO ()
main = do
  doMonadTrans

goodMain :: IO ()
goodMain = do
    opts <- execParser handleOpts
    HTTP.withManager HTTP.defaultManagerSettings $ \m -> do
      let env = Env { envOpts    = opts
                    , envManager = m
                    }

      runReaderT (runClient handleCmd) env
  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )

handleCmd :: Client ()
handleCmd = do
  env <- ask
  let opts = envOpts env
  case (optsCommand opts) of
    JobStatuses          -> Jen.jobStatuses        >>= mapM_ (liftIO . putStrLn . show)
    (JobStatus jobId)    -> Jen.jobStatus jobId    >>= liftIO .  putStrLn . show
    (RunBuild jobId rev) -> Jen.runBuild jobId rev >>  liftIO (putStrLn "OK")
    (BuildLog jobId mBn) -> Jen.buildLog jobId mBn
