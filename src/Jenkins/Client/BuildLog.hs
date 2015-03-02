module Jenkins.Client.BuildLog
  ( buildLog
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.CaseInsensitive as CI

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent

import Network.HTTP.Client
import Network.HTTP.Types

import Jenkins.Client.Types
import Jenkins.Types hiding (jobStatus)
import qualified Jenkins.Endpoints as JEP

buildLog :: T.Text
         -> Maybe BuildNum
         -> Client ()
buildLog name (Just bn) = stream Nothing name bn
buildLog name Nothing   = do
  req <- JEP.getJob name
  env <- ask
  handlingFailures req $ \(JobWithBuildNums _ nums) -> do
    case nums of
      []     -> putStrLn "This job has no builds yet."
      (bn:_) -> runClient env $ stream Nothing name bn

------------------------------------------------------------------------------------------------------------------------

stream :: Maybe Int
       -> T.Text
       -> BuildNum
       -> Client ()
stream mStart job buildNum = do
  req'                <- JEP.buildLog mStart job buildNum
  m                   <- manager
  (mLength, moreData) <- liftIO $ withResponse req' m $ \resp -> do
    responseBody resp >>= BS.putStrLn
    let hs = responseHeaders $ resp
    return ( contentLength hs, hasMoreData hs)

  let justStart = Just $ maybe 0 id mStart
      mStart'   = (+) <$> justStart <*> mLength
  case (mLength, moreData) of
    (Just _, True) -> sleep >> stream mStart' job buildNum
    _              -> return ()

sleep :: Client ()
sleep = liftIO $ threadDelay (5 * 1000000)

hasMoreData :: [Header] -> Bool
hasMoreData hs =
  let k = "X-More-Data" :: CI.CI BS.ByteString
  in L.any (\h -> fst h == k) hs

contentLength :: [Header] -> Maybe Int
contentLength hs =
  let k = "Content-Length" :: CI.CI BS.ByteString
      v = L.find (\h -> fst h == k ) hs
  in fmap (read . BS.unpack . snd) v
