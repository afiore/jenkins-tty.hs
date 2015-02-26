module Jenkins.Endpoints
  ( getJobs
  , getJob
  , getBuild
  , runBuild
  , buildLog
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as LBS

import System.FilePath ((</>))
import Control.Monad.Trans

import Network.HTTP.Client (Request(..), parseUrl, setQueryString, applyBasicAuth)
import Network.HTTP.Types

import Options

import Jenkins.Types
import Jenkins.Client.Types

getJobs :: Client Request
getJobs = defaultReq ""

getJob :: T.Text -> Client Request
getJob name = defaultReq ("job" </> T.unpack name)

getBuild :: T.Text -> BuildNum -> Client Request
getBuild job (BuildNum n) =
  defaultReq ("job" </> T.unpack job </> show n)

runBuild :: T.Text
         -> Maybe T.Text
         -> Client Request
runBuild job rev = do
  req <- defaultReq ("job" </> T.unpack job </> "buildWithParameters")
  let rev' = fmap LBS.encodeUtf8 rev
      q    = [("GIT_REV", rev')]
  return (setQueryString q req) { method = methodPost }

buildLog :: T.Text -> BuildNum -> Client Request
buildLog job (BuildNum n) = do
  let q = [("start", Just "0")]
  req <- defaultReq ("job" </> T.unpack job </> show n </> "logText" </> "progressiveText")
  return $ setQueryString q req
------------------------------------------------------------

defaultReq :: String -> Client Request
defaultReq p = do
  baseUri <- option optsBaseUri
  mCreds  <- option optsAuth
  let url  = baseUri </> p </> apiSuffix
  req     <- liftIO $ parseUrl url
  return $ case mCreds of
             Just (user, pass) -> applyBasicAuth user pass req
             _                 -> req

apiSuffix :: String
apiSuffix = "api" </> "json"

