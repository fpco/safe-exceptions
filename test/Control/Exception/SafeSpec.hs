module Control.Exception.SafeSpec (spec) where

import Control.Exception
import Control.Exception.Safe
import Test.Hspec

spec :: Spec
spec = do
    describe "isSyncException" $ do
        let test e expected = it (show e) (isSyncException e `shouldBe` expected)
        test Overflow True
        test ThreadKilled False
    describe "toSyncException" $ do
        let test e = it (show e) (isSyncException (toSyncException e) `shouldBe` True)
        test Overflow
        test ThreadKilled
    describe "toAsyncException" $ do
        let test e = it (show e) (isAsyncException (toAsyncException e) `shouldBe` True)
        test Overflow
        test ThreadKilled
