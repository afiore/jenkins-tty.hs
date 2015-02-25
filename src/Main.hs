module Main where

import Options.Applicative

import qualified Jenkins.Client as Jen
import qualified Network.HTTP.Client as HTTP
import Jenkins.Blah
import Options

handleCmd :: HTTP.Manager -> Command -> IO ()
handleCmd m JobStatuses          = Jen.jobStatuses m >>= mapM_ (putStrLn . show)
handleCmd m (JobStatus jobId)    = Jen.jobStatus m jobId >>= putStrLn . show
handleCmd m (RunBuild jobId rev) = Jen.runBuild m jobId rev >> putStrLn "OK.."
handleCmd m (BuildLog jobId mBn) = Jen.buildLog m jobId mBn

main :: IO ()
main = do
  doMonadTrans

goodMain :: IO ()
goodMain = do
    opts <- execParser handleOpts
    HTTP.withManager HTTP.defaultManagerSettings $ \m -> do
      handleCmd m $ optCommand opts
  where
    handleOpts = info (helper <*> parseOptions)
          ( fullDesc
          <> progDesc "A cli tool for managing Jenkins' builds"
          <> header "jenkins-tty - a command line interface for Jenkins" )
