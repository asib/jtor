module BEncode.Internal
  ( number
  , string
  , integer
  , list
  , dict
  , value
  , run
  , Value(..)
  , encode
  ) where

import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP ((+++))
import qualified Data.Char as C
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Protolude (lastMay)

data Value = BString String
           | BInt Integer
           | BList [Value]
           | BDict (M.Map String Value)
           deriving (Eq, Show)

encode :: Value -> String
encode (BString s) = (show $ length s) ++ ":" ++ s
encode (BInt n) = "i" ++ show n ++ "e"
encode (BList vs) = "l" ++ (foldl (\acc v -> acc ++ encode v) "" vs) ++ "e"
encode (BDict m) = "d" ++ (M.foldrWithKey (\k v acc -> acc ++ encode (BString k) ++ encode v) "" m) ++ "e"

-- Can't read '0'
number :: (Read a, Num a) => P.ReadP a
number =  do
  minus <- P.option '+' $ P.char '-'
  let sign = case minus of
              '+' -> 1
              '-' -> -1
  v <- P.satisfy $ \c -> C.isDigit c && c /= '0'
  vs <- P.many $ P.satisfy C.isDigit
  maybe P.pfail (return . (*sign)) $ readMaybe (v:vs)

string :: P.ReadP String
string = do
  len <- number
  P.char ':'
  val <- sequence $ replicate len P.get
  return val

integer :: P.ReadP Integer
integer = P.char 'i' *> (number +++ (P.char '0' >> return 0)) <* P.char 'e'

list :: P.ReadP [Value]
list = P.char 'l' *> P.many1 value <* P.char 'e'

dict :: P.ReadP [(String, Value)]
dict = do
  P.char 'd'
  kvs <- P.many1 $ do
    k <- string
    v <- value
    return (k, v)
  P.char 'e'
  return kvs

value :: P.ReadP Value
value = (BString <$> string) +++ (BInt <$> integer) +++ (BList <$> list) +++ (BDict . M.fromList <$> dict)

run :: P.ReadP a -> String -> Maybe a
run p = fmap fst . lastMay . P.readP_to_S p
