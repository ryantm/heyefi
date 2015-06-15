{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HEyefi.Types where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Reader.Class (MonadReader)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

--Global Monads
newtype HEyefiM a = HEyefiM {
  runHeyefi :: ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

-- Logging
data LogLevel = Info | Debug
              deriving (Eq, Show)

-- Configuration
type MacAddress = Text
type UploadKey = Text

type CardConfig = HM.HashMap MacAddress UploadKey

data Config = Config {
  cardMap :: CardConfig,
  uploadDirectory :: FilePath,
  logLevel :: LogLevel
}

type SharedConfig = TVar Config
