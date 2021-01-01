module BEncode where

import qualified Data.Text as T
import Data.ByteString (ByteString)

import BEncode.Internal

decode :: ByteString -> Either String BValue
decode = run value

decodeMaybe :: ByteString -> Maybe BValue
decodeMaybe = rightToMaybe . run value
