module Token where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.Trifecta

p' :: Parser [Integer]
p' = some $ do
  i <- token digit
  return (read [i])
