{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (main, spec) where

import           Test.Hspec

import qualified Data.Map as Map
import           Network.Socket

import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatSockAddr" $ do
    context "when used with an IPv4 address" $ do
      it "converts it to a String" $ do
        addr <- SockAddrInet 4040 <$> inet_addr "127.0.0.1"
        formatSockAddr addr `shouldReturn` "127.0.0.1"
    context "when used with an IPv6 address" $ do
      it "converts it to a String" $ do
        let addr = SockAddrInet6 4040 0 (0, 0, 65535, 2130706433) 0
        formatSockAddr addr `shouldReturn` "::ffff:127.0.0.1"

  describe "addForwardedForHeader" $ do
    it "adds X-Forwarded-For Header" $ do
      addForwardedForHeader "127.0.0.1" Map.empty `shouldBe` Map.fromList [("X-Forwarded-For", "127.0.0.1")]

    context "when X-Forwarded-For Header is already present" $ do
      it "augments it" $ do
        let headers = Map.fromList [("X-Forwarded-For", "172.16.254.1")]
        addForwardedForHeader "127.0.0.1" headers `shouldBe` Map.fromList [("X-Forwarded-For", "172.16.254.1, 127.0.0.1")]

  describe "removeConnectionHeader" $ do
    it "removes Connection header" $ do
      removeConnectionHeader [("Connection", "close"), ("Content-Type", "text/html")] `shouldBe` [("Content-Type", "text/html")]

  describe "isConnectionClose" $ do
    it "is True if 'Connection: close' is given" $ do
      isConnectionClose [("Connection", "close")] `shouldBe` True

    it "ignores whitespace" $ do
      isConnectionClose [("Connection", " close  ")] `shouldBe` True

    it "ignores case" $ do
      isConnectionClose [("Connection", "Close")] `shouldBe` True

    it "is False if 'Connection: close' is not given" $ do
      isConnectionClose [("Connection", "foo")] `shouldBe` False
