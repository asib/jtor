{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import qualified BEncode.Internal as I
import Data.ByteString.Char8 (ByteString, pack, concat, replicate)
import qualified Data.ByteString.Char8 as C
import Prelude hiding (concat, replicate)

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary

number :: Spec
number = describe "number parser" $ do
  it "parses a number" $ do
    property $ \(n :: Integer) -> I.runMaybe I.number (pack $ show n) == Just n
  it "rejects '-0'" $ do
    I.runMaybe I.number "-0" `shouldBe` Nothing
  it "rejects leading zeros" $ do
    property $ \(sign :: Bool, m :: Int, n :: Integer) -> m > 0 && n > 0 ==> I.runMaybe I.number (concat [(if sign then "" else "-"), replicate m '0', pack $ show n]) == Nothing

integer :: Spec
integer = describe "integer parser" $ do
  it "accepts 0" $ do
    I.runMaybe I.integer "i0e" `shouldBe` Just 0
  it "accepts a number" $ do
    property $ \n -> I.runMaybe I.integer (concat ["i", pack $ show n, "e"]) == Just n

string :: Spec
string = describe "string parser" $ do
  it "parses a valid string" $ do
    property $ \xs -> C.length xs > 0 ==> I.runMaybe I.string (concat [(pack . show $ C.length xs), ":", xs]) == Just xs
  it "rejects empty string" $ do
    I.runMaybe I.string "" `shouldBe` Nothing

list :: Spec
list = describe "list parser" $ do
  it "rejects empty list" $ do
    I.runMaybe I.list "le" `shouldBe` Nothing
  it "accepts a list of integers" $ do
    property $ \(xs :: [Integer]) -> length xs > 0 ==> I.runMaybe I.list (I.encode $ I.BList $ I.BInt <$> xs) == Just (I.BInt <$> xs)
  it "accepts a list of strings" $ do
    property $ \(xs :: [ByteString]) -> length xs > 0 && all ((>0) . C.length) xs ==> I.runMaybe I.list (I.encode $ I.BList $ I.BString <$> xs) == Just (I.BString <$> xs)
  it "accepts a list with a string and an integer" $ do
    I.runMaybe I.list "li34e4:spame" `shouldBe` Just [I.BInt 34, I.BString "spam"]
  it "accepts a list with an integer and a string" $ do
    I.runMaybe I.list "l4:spami34ee" `shouldBe` Just [I.BString "spam", I.BInt 34]

dict :: Spec
dict = describe "dict parser" $ do
  it "rejects empty dict" $ do
    I.runMaybe I.dict "de" `shouldBe` Nothing
  it "accepts a dictionary with some values" $ do
    I.runMaybe I.dict "d3:numi35e4:word7:haskelle" `shouldBe` Just [("num", I.BInt 35), ("word", I.BString "haskell")]


main :: IO ()
main = hspec $ do
  describe "decode" $ do
    number
    integer
    string
    list
    dict

