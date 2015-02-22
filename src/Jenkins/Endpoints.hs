module Jenkins.Endpoints
  ( getJobs
  , getJob
  , getBuild
  , runBuild
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as LBS

import System.FilePath ((</>))
import Network.HTTP.Client (Request(..), parseUrl, setQueryString)
import Network.HTTP.Types

import Jenkins.Types

getJobs :: IO Request
getJobs = parseUrl (apiUrl "")

getJob :: T.Text -> IO Request
getJob name = parseUrl $ apiUrl ("job" </> T.unpack name)

getBuild :: T.Text -> BuildNum -> IO Request
getBuild job (BuildNum n) =
  parseUrl $ apiUrl ("job" </> T.unpack job </> show n)

runBuild :: T.Text
         -> Maybe T.Text
         -> IO Request
runBuild job rev = do
  let path' = apiUrl ("job" </> T.unpack job </> "buildWithParameters")
      rev'  = fmap LBS.encodeUtf8 rev
      q     = [("GIT_REV", rev')]
  req <- parseUrl path'
  return (setQueryString q req) { method = methodPost }

------------------------------------------------------------

defaultHost :: String
defaultHost = "http://localhost:8080"

apiSuffix :: String
apiSuffix = "api/json"

apiUrl :: String -> String
apiUrl p = defaultHost </> p </> apiSuffix