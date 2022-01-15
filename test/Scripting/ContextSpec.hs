{-# LANGUAGE QuasiQuotes #-}

module Scripting.ContextSpec where

import Control.Error
import qualified Data.Text as T
import NeatInterpolation
import Scripting.Context
import Test.Hspec

spec :: Spec
spec = do
  describe "Values" $ do
    context "when provided with an StringValue" $ do
      let stringValue = StringValue "something"
          numberValue = StringValue "12.34"

      describe "valueAsString" $ do
        it "should return value as-is" $ do
          valueAsString stringValue `shouldBe` Just "something"
          valueAsString numberValue `shouldBe` Just "12.34"

      describe "valueAsNumber" $ do
        it "should convert to decimal, if possible" $ do
          valueAsDecimal numberValue `shouldBe` Just 12.34
          valueAsDecimal stringValue `shouldBe` Nothing

    context "when provided with a NumberValue" $ do
      let value = NumberValue 12.34

      describe "valueAsString" $ do
        it "should convert number values as string" $
          valueAsString value `shouldBe` Just "12.34"

      describe "valueAsNumber" $ do
        it "should return value as-is" $
          valueAsDecimal value `shouldBe` Just 12.34

  describe "evaluate" $ do
    it "should expose resulted variables values" $ do
      let ctx = evaluate "a = \"3.3\" + 2 - -1 * 2 / 3"
      fmapR (variable "a") ctx `shouldBe` Right (Just (NumberValue 5.96667))

    it "should fail with 'not a number' when a non number string is used in expressions" $ do
      evaluate "a = \"p\" + 2" `shouldBe` Left "not a number"
      evaluate "a = 1 + \"p\"" `shouldBe` Left "not a number"
      evaluate "a = -\"p\"" `shouldBe` Left "not a number"

    it "should fail with 'variable X not defined' when an undefined variable is used in an expression" $ do
      evaluate "a = b + 2" `shouldBe` Left "Variable 'b' not defined"
      evaluate "a = 1 + b" `shouldBe` Left "Variable 'b' not defined"
      evaluate "a = -b + c" `shouldBe` Left "Variable 'b' not defined"

    it "should support multiple operations" $ do
      let ctx =
            evaluate
              ( T.unpack
                  [trimming|
                    a = 1 + 2 + 3
                    b = a * 2
                  |]
              )
      fmapR (variable "a") ctx `shouldBe` Right (Just (NumberValue 6))
      fmapR (variable "b") ctx `shouldBe` Right (Just (NumberValue 12))
