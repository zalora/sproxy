{-# LANGUAGE OverloadedStrings #-}
module CookiesSpec (main, spec) where

import           Test.Hspec

import           Cookies

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "setCookie" $ do
    it "sets a cookie" $ do
      setCookie "example.com" "sproxy" "foo" (30 * 24 * 60 * 60) `shouldBe` "sproxy=foo; Max-Age=2592000; Domain=example.com; HttpOnly; Secure"

  describe "invalidateCookie" $ do
    it "invalidates a cookie" $ do
      invalidateCookie "example.com" "sproxy" `shouldBe` "sproxy=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; Domain=example.com; HttpOnly; Secure"

  describe "formatCookies" $ do
    it "formats a given list of cookies" $ do
      formatCookies [("foo", "23"), ("bar", "42")] `shouldBe` "foo=23; bar=42"

  describe "parseCookies" $ do
    it "parses Cookie header" $ do
      parseCookies [("Cookie", "foo=23; bar=42")] `shouldBe`
        [("foo", "23"), ("bar", "42")]

    context "when no Cookie header is present" $ do
      it "returns an empty list" $ do
        parseCookies [("", "")] `shouldBe` []

    context "when Cookie header is malformed" $ do
      it "returns an empty list" $ do
        parseCookies [("Cookie", "foo")] `shouldBe` []

  describe "removeCookie" $ do
    let cookies = [("foo", "23"), ("bar", "42")]
    it "removes cookie with specified name from given list of cookies" $ do
      removeCookie "foo" cookies `shouldBe` Just ("23", [("bar", "42")])

    context "when there is no cookie with specified name" $ do
      it "returns Nothing" $ do
        removeCookie "baz" cookies `shouldBe` Nothing
