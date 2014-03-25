module CookiesSpec (main, spec) where

import           Test.Hspec

import           Cookies
import           Network.HTTP.Cookie

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseCookies" $ do
    it "parses Cookie header" $ do
      parseCookies "www.example.com" [("Cookie", "name=value; name2=value2")] `shouldBe`
        [cookie "www.example.com" "name" "value", cookie "www.example.com" "name2" "value2"]

    context "when no Cookie header is present" $ do
      it "returns an empty list" $ do
        parseCookies "www.example.com" [("", "")] `shouldBe` []

    context "when Cookie header is malformed" $ do
      it "returns an empty list" $ do
        parseCookies "www.example.com" [("Cookie", "foo")] `shouldBe` []
  where
    cookie domain name value = MkCookie domain name value Nothing Nothing Nothing
