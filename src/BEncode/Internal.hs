{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module BEncode.Internal where

import Prelude hiding (concat, length)
import Text.Read (readMaybe)
import Control.Applicative
import Control.Lens
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Attoparsec.ByteString.Char8 ( Parser
                                        , char
                                        , decimal
                                        , many1
                                        , count
                                        , anyChar
                                        , parseOnly
                                        )
import Data.ByteString.Char8 ( ByteString
                             , pack
                             , concat
                             , append
                             , length
                             )


data BValue = BString ByteString
            | BInt Integer
            | BList [BValue]
            | BDict (M.Map ByteString BValue)
            deriving (Eq, Show)

$(makePrisms ''BValue)

-- fetch :: BValue -> ByteString -> Traversal' BValue a -> Maybe a
fetch v k p = v ^? _BDict . at k . _Just . p


encode :: BValue -> ByteString
encode (BString s) = concat [(pack . show $ length s), ":", s]
encode (BInt n) = concat ["i", pack $ show n, "e"]
encode (BList vs) = concat ["l", (foldl (\acc v -> acc `append` encode v) "" vs), "e"]
encode (BDict m) = concat ["d", (M.foldrWithKey (\k v acc -> concat [acc, encode (BString k), encode v]) "" m), "e"]

number :: (Integral a) => Parser a
number = (negate <$> (char '-' *> decimal >>= \n -> if n == 0 then fail "-0 not valid" else pure n))
  <|> decimal

nonzero :: (Integral a) => Parser a
nonzero = number >>= \n -> if n == 0 then fail "expected non-zero" else pure n

string :: Parser ByteString
string = fmap pack $ nonzero <* char ':' >>= flip count anyChar

integer :: Parser Integer
integer = char 'i' *> number <* char 'e'

list :: Parser [BValue]
list = char 'l' *> many1 value <* char 'e'

dict :: Parser [(ByteString, BValue)]
dict = do
  char 'd'
  kvs <- many1 $ do
    k <- string
    v <- value
    return (k, v)
  char 'e'
  return kvs

value :: Parser BValue
value = (BString <$> string) <|> (BInt <$> integer) <|> (BList <$> list) <|> (BDict . M.fromList <$> dict)

run = parseOnly
runMaybe p = rightToMaybe . run p
rightToMaybe (Left _) = Nothing
rightToMaybe (Right a) = Just a
