module HEyefi.ConfigSpec where

import           Test.Hspec

import           HEyefi.Config
import           HEyefi.Prelude
import           HEyefi.Types (cardMap, uploadDirectory, Config)

import           Control.Exception (catch, SomeException, throwIO)
import qualified Data.HashMap.Strict as HM
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.FilePath ((</>))
import           System.IO.Error (isDoesNotExistError)
import           System.IO.Silently (capture)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

getNonexistentTemporaryFile :: IO FilePath
getNonexistentTemporaryFile = do
  tempdir <- catch getTemporaryDirectory (\(_::SomeException) -> return ".")
  let file = tempdir </> "heyefi.config"
  removeIfExists file
  return file

makeAndReloadFile_ :: Text -> IO Text
makeAndReloadFile_ s = fmap fst (makeAndReloadFile s)

makeAndReloadFile :: Text -> IO (Text, Config)
makeAndReloadFile contents = do
  file <- getNonexistentTemporaryFile
  writeFile file contents
  (a,b) <- capture (do
                       r <- runWithEmptyConfig (reloadConfig file)
                       return (fst r))
  return (pack a, b)

validConfig :: Text
validConfig = "upload_dir = \"/data/photos\"\ncards = [[\"0012342de4ce\",\"e7403a0123402ca062\"],[\"1234562d5678\",\"12342a062\"]]"

spec :: Spec
spec =
  describe "reloadConfig" (do
    it "should report an error for a non-existent configuration file"
     (do
         file <- getNonexistentTemporaryFile
         (output, config) <- capture (
           do
             r <- runWithEmptyConfig (reloadConfig file)
             return (fst r))
         cardMap config `shouldBe` HM.empty
         output `shouldContain` "Could not find configuration file at " ++ file)
    it "should report an error for an unparsable configuration file"
     (do
         (output, config) <- makeAndReloadFile "a = (\n"
         cardMap config `shouldBe` HM.empty
         unpack output `shouldContain` "Error parsing configuration file at "
         unpack output `shouldContain` "with message: endOfInput")
    it "should complain about missing cards configuration"
     (do
         (output, config) <- makeAndReloadFile "upload_dir = \"/data/annex/doxie/unsorted\""
         cardMap config `shouldBe` HM.empty
         unpack output `shouldContain` "missing a definition for `cards`.")
    it "should complain about cards not having the correct format"
     (do
         (_, config) <- makeAndReloadFile "upload_dir = \"/data/annex/doxie/unsorted\"\ncards=[[\"1\",\"2\",\"3\"]]"
         cardMap config `shouldBe` HM.empty
         output2 <- makeAndReloadFile_ "upload_dir = \"/data/annex/doxie/unsorted\"\ncards=\"1\""
         unpack output2 `shouldContain` "Format of cards does not match")
    it "should complain about missing upload_dir configuration"
     (do
         (output, config) <-  makeAndReloadFile "cards = [[\"0012342de4ce\",\"e7403a0123402ca062\"],[\"1234562d5678\",\"12342a062\"]]"
         uploadDirectory config `shouldBe` ""
         unpack output `shouldContain`"missing a definition for `upload_dir`.")
    it "should parse cards for a valid configuration"
     (do
         (_, config) <- makeAndReloadFile validConfig
         uploadDirectory config `shouldBe` "/data/photos"
         HM.lookup "0012342de4ce" (cardMap config) `shouldBe` Just "e7403a0123402ca062"
         HM.lookup "1234562d5678" (cardMap config) `shouldBe` Just "12342a062"))
