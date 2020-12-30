module BEncode
    ( Value(..)
    , decode
    ) where

import qualified Data.Text as T

import BEncode.Internal

decode :: T.Text -> Maybe Value
decode = run value . T.unpack
