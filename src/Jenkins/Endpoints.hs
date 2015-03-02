module Jenkins.Endpoints
  ( getJobs
  , getJob
  , getBuild
  , runBuild
  , buildLog
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE

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
  req <- postReq ("job" </> T.unpack job </> "buildWithParameters")
  let rev' = fmap TE.encodeUtf8 rev
      q    = [("GIT_REV", rev')]
  return (setQueryString q req)

buildLog :: Maybe Int -> T.Text -> BuildNum -> Client Request
buildLog mStart job (BuildNum n) = do
  let q = maybe [] (\s -> [("start", Just (BS.pack $ show s))]) mStart
      p = ("job" </> T.unpack job </> show n </> "logText" </> "progressiveText")
  req <- defaultReq p
  return $ setQueryString q req

defaultReq :: String -> Client Request
defaultReq p = do
  baseUri <- option optsBaseUri
  mCreds  <- option optsAuth
  req     <- liftIO . parseUrl $ baseUri </> p </> apiSuffix
  return $ case mCreds of
             Just (user, pass) -> applyBasicAuth user pass req
             _                 -> req

postReq :: String -> Client Request
postReq p = do
  req <- defaultReq p
  return $ req { method = methodPost }

apiSuffix :: String
apiSuffix = "api" </> "json"
