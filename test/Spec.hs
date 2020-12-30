{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import qualified BEncode.Internal as I

instance Arbitrary I.Value where
  arbitrary =
    oneof
      [ I.BString <$> listOf1 arbitrary
      , I.BInt <$> arbitrary
      , I.BList <$> listOf1 arbitrary
      , I.BDict <$> arbitrary
      ]

number :: Spec
number = describe "number parser" $ do
  it "parses a number" $ do
    property $ \(n :: Integer) -> n /= 0 ==> I.run I.number (show n) == Just n
  it "rejects '-0'" $ do
    I.run I.number "-0" `shouldBe` Nothing
  it "rejects leading zeros" $ do
    property $ \(sign :: Bool, m :: Int, n :: Integer) -> m > 0 && n > 0 ==> I.run I.number ((if sign then "" else "-") ++ replicate m '0' ++ show n) == Nothing

integer :: Spec
integer = describe "integer parser" $ do
  it "accepts 0" $ do
    I.run I.integer "i0e" `shouldBe` Just 0
  it "accepts a number" $ do
    property $ \n -> I.run I.integer ("i" ++ show n ++ "e") == Just n

string :: Spec
string = describe "string parser" $ do
  it "parses a valid string" $ do
    property $ \xs -> length xs > 0 ==> I.run I.string ((show $ length xs) ++ ":" ++ xs) == Just xs
  it "rejects empty string" $ do
    I.run I.string "" `shouldBe` Nothing

list :: Spec
list = describe "list parser" $ do
  it "rejects empty list" $ do
    I.run I.list "le" `shouldBe` Nothing
  it "accepts a list of integers" $ do
    property $ \(xs :: [Integer]) -> length xs > 0 ==> I.run I.list (I.encode $ I.BList $ I.BInt <$> xs) == Just (I.BInt <$> xs)
  it "accepts a list of strings" $ do
    property $ \(xs :: [String]) -> length xs > 0 && all ((>0) . length) xs ==> I.run I.list (I.encode $ I.BList $ I.BString <$> xs) == Just (I.BString <$> xs)
  it "accepts a list with a string and an integer" $ do
    I.run I.list "li34e4:spame" `shouldBe` Just [I.BInt 34, I.BString "spam"]
  it "accepts a list with an integer and a string" $ do
    I.run I.list "l4:spami34ee" `shouldBe` Just [I.BString "spam", I.BInt 34]

dict :: Spec
dict = describe "dict parser" $ do
  it "rejects empty dict" $ do
    I.run I.dict "de" `shouldBe` Nothing
  it "accepts a dictionary with some values" $ do
    I.run I.dict "d3:numi35e4:word7:haskelle" `shouldBe` Just [("num", I.BInt 35), ("word", I.BString "haskell")]


main :: IO ()
main = hspec $ do
  describe "decode" $ do
    number
    integer
    string
    list
    dict

