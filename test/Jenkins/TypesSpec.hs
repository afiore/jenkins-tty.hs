module Jenkins.TypesSpec
  ( test
  ) where

import Test.Hspec
import Jenkins.Types

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

import System.FilePath

parseResource :: FromJSON a => FilePath -> IO (Either String a)
parseResource res = do
  fbody <- LBS.readFile ("test" </> "resources" </> "json" </> res )
  return $ eitherDecode fbody

test = do
  describe "JobList" $ do
    it "parses a job list" $ do
      jobs <- parseResource "job-statuses.json"
      (fmap fromJobList jobs) `shouldBe` Right [ Job "success-app" JobSuccess
                                               , Job "failure-app" JobFailure
                                               , Job "in-progress-app" JobInProgress
                                               ]
  describe "JobWithBuildNums" $ do
    it "parses a Job with a list of build numbers" $ do
      jwbs <- parseResource "job-with-builds.json"
      jwbs `shouldBe` Right (JobWithBuildNums
                               (Job "test-app" JobFailure)
                               (map BuildNum (reverse [1..5])))

  describe "RawBuild" $ do
    context "when build is completed" $ do
      it "parses a RawBuild" $ do
        let sha     = "070cb5c898cf045020923c381de35304461844d1"
            actions = [ OtherAction
                      , OtherAction
                      , LastBuiltRev sha
                                     [Branch "origin/schools"]
                      , OtherAction
                      , OtherAction
                      , OtherAction
                      ]

        rawBuild <- parseResource "raw-build.json"
        rawBuild `shouldBe` Right (RawBuild
                                  { rawBuildNumber = BuildNum 25
                                  , rawBuildResult = JobSuccess
                                  , rawBuildTimestamp = 1423653757651
                                  , rawBuildDuration = 87212
                                  , rawBuildActions  = actions
                                  })

    context "when build is in progress" $ do
      it "parses a rawbuild with the InProgress result" $ do
        let actions =[ OtherAction
                     , OtherAction
                     , LastBuiltRev "2fc158d4779616ece8f4f75461746c08fbb48c09"
                                    [Branch "origin/master"]
                     ]

        rawBuild <- parseResource "build-in-progress.json" :: IO (Either String RawBuild)
        rawBuild `shouldBe` Right (RawBuild
                                  { rawBuildNumber = BuildNum 8
                                  , rawBuildResult = JobInProgress
                                  , rawBuildTimestamp = 1425215442169
                                  , rawBuildDuration = 0
                                  , rawBuildActions = actions



                                  })
