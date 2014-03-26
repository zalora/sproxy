module CookiesSpec (main, spec) where

import           Test.Hspec

import           Cookies
import           Network.HTTP.Cookie

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatCookies" $ do
    it "formats a given list of cookies" $ do
      formatCookies [cookie "www.example.com" "foo" "23", cookie "www.example.com" "bar" "42"] `shouldBe` "foo=23; bar=42"

  describe "parseCookies" $ do
    it "parses Cookie header" $ do
      parseCookies "www.example.com" [("Cookie", "foo=23; bar=42")] `shouldBe`
        [cookie "www.example.com" "foo" "23", cookie "www.example.com" "bar" "42"]

    context "when no Cookie header is present" $ do
      it "returns an empty list" $ do
        parseCookies "www.example.com" [("", "")] `shouldBe` []

    context "when Cookie header is malformed" $ do
      it "returns an empty list" $ do
        parseCookies "www.example.com" [("Cookie", "foo")] `shouldBe` []

  describe "removeCookie" $ do
    let cookies = [
            cookie "www.example.com" "foo" "23"
          , cookie "www.example.com" "bar" "42"
          ]
    it "removes cookie with specified name from given list of cookies" $ do
      removeCookie "foo" cookies `shouldBe` Just (cookie "www.example.com" "foo" "23", [cookie "www.example.com" "bar" "42"])

    context "when there is no cookie with specified name" $ do
      it "returns Nothing" $ do
        removeCookie "baz" cookies `shouldBe` Nothing
  where
    cookie domain name value = MkCookie domain name value Nothing Nothing Nothing
