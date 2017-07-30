module HEyefi.Prelude (
  module X,
  Text,
  tshow,
  tmd5,
  tlength,
  terror,
  T.pack,
  T.unpack,
  T.writeFile,
  (<>)) where

import           Control.Category as X
import           Data.Hash.MD5 (md5s, Str (..))
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           HEyefi.Texts as X
import qualified Prelude as X hiding (String, writeFile, (.), id)
import           Prelude hiding ((.))

tshow :: Show a => a -> Text
tshow = show >>> T.pack

tlength :: Text -> Int
tlength = T.length

terror :: Text -> a
terror = T.unpack >>> error

tmd5 :: Text -> Text
tmd5  = T.unpack >>> Str >>> md5s >>> T.pack
