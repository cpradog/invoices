module Scripting.ContextSpec where

import Control.Lens
import qualified Data.Map as M
import GHC.IO (IO)
import Scripting.Context
import Test.Hspec

spec :: Spec
spec = do
  describe "valueAsString" $ do
    it "should return string value as-is" $
      valueAsString (Just (StringValue "something")) `shouldBe` Just "something"

    it "should convert number values as string" $
      valueAsString (Just (NumberValue 12.34)) `shouldBe` Just "12.34"

  describe "valueAsNumber" $ do
    it "should return Nothing for string values" $ do
      valueAsDecimal (Just (StringValue "12.34")) `shouldBe` Nothing
      valueAsDecimal (Just (StringValue "something")) `shouldBe` Nothing

    it "should return decimal value as-is" $
      valueAsDecimal (Just (NumberValue 12.34)) `shouldBe` Just 12.34

  describe "evaluate" $ do
    it "should expose resulted variables values" $ do
      let ctx = evaluate "a = \"1\" + 2 + 3"
      variable "a" ctx `shouldBe` Just (NumberValue 6)
