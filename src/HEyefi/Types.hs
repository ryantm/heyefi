{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HEyefi.Types where

import           Control.Concurrent.STM (TVar)
import           Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Lazy (StateT)
import           Control.Monad.State.Class (MonadState)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           Network.Wai (Request, Response, ResponseReceived)

--Global Monads
newtype HEyefiM a = HEyefiM {
  runHeyefi :: StateT Config IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadMask
             , MonadCatch
             , MonadThrow
             , MonadState Config)

type HEyefiApplication = Request
                         -> (Response -> IO ResponseReceived)
                         -> HEyefiM ResponseReceived

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
  logLevel :: LogLevel,
  lastSNonce :: String
}

type SharedConfig = TVar Config

data SoapAction = StartSession
                | GetPhotoStatus
                | MarkLastPhotoInRoll
                deriving (Show, Eq)
