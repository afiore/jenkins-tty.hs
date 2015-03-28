module Jenkins.Endpoints
  ( getJobs
  , getJob
  , getBuild
  , runBuild
  , buildLog
  ) where

import qualified Data.Text as T

import System.FilePath ((</>))
import Control.Monad.Trans
import Control.Arrow

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
         -> BuildParams
         -> Client Request
runBuild job (BuildParams params) = do
  req <- postReq ("job" </> T.unpack job </> "buildWithParameters")
  let q = map (second Just) params
  return $ setQueryString q req

buildLog :: T.Text -> BuildNum -> Client Request
buildLog job (BuildNum n) = do
  let p = "job" </> T.unpack job </> show n </> "logText" </> "progressiveText"
  defaultReq p

defaultReq :: String -> Client Request
defaultReq p = do
  baseUri <- option optsBaseUri
  mCreds  <- option optsAuth
  req     <- liftIO . parseUrl $ baseUri </> p </> apiSuffix
  let cs _ _ _ = Nothing
      req'     = req { checkStatus = cs }
  return $ case mCreds of
             Just (AuthCreds (user, pass)) -> applyBasicAuth user pass req'
             _                             -> req'

postReq :: String -> Client Request
postReq p = do
  req <- defaultReq p
  return $ req { method = methodPost }

apiSuffix :: String
apiSuffix = "api" </> "json"
