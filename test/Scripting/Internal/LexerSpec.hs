module Scripting.Internal.LexerSpec where

import Scripting.Internal.Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "tokens" $ do
    it "should ignore white spaces" $ do
      tokens " \t \r \n \r\n" `shouldBe` []

    it "should ignore line comments" $ do
      tokens "# some comment" `shouldBe` []

    it "should report unknown tokens" $ do
      tokens "1a" `shouldBe` [TUnknown (1, 1) "1a"]

    it "should report quoted strings" $ do
      tokens "\"\"" `shouldBe` [TString (1, 1) ""]
      tokens "\"some string\"" `shouldBe` [TString (1, 1) "some string"]
      tokens "\"some \\\" escaped string\"" `shouldBe` [TString (1, 1) "some \\\" escaped string"]
      tokens "\"some\" \"string\"" `shouldBe` [TString (1, 1) "some", TString (1, 8) "string"]

    it "should report numbers" $ do
      tokens "1234" `shouldBe` [TNumber (1, 1) "1234"]
      tokens "12.34" `shouldBe` [TNumber (1, 1) "12.34"]
      tokens "12 345.67" `shouldBe` [TNumber (1, 1) "12345.67"]
      tokens "12_345.67" `shouldBe` [TNumber (1, 1) "12345.67"]

    it "should report symbols" $ do
      tokens "+" `shouldBe` [TSymbol (1, 1) "+"]
      tokens "-" `shouldBe` [TSymbol (1, 1) "-"]
      tokens "*" `shouldBe` [TSymbol (1, 1) "*"]
      tokens "/" `shouldBe` [TSymbol (1, 1) "/"]
      tokens "=" `shouldBe` [TSymbol (1, 1) "="]
      tokens "(" `shouldBe` [TSymbol (1, 1) "("]
      tokens ")" `shouldBe` [TSymbol (1, 1) ")"]
      tokens "[" `shouldBe` [TSymbol (1, 1) "["]
      tokens "]" `shouldBe` [TSymbol (1, 1) "]"]
      tokens "{" `shouldBe` [TSymbol (1, 1) "{"]
      tokens "}" `shouldBe` [TSymbol (1, 1) "}"]

    it "should report recognized keywords" $ do
      tokens "reference" `shouldBe` [TKeyword (1, 1) "reference"]
      tokens "tax" `shouldBe` [TKeyword (1, 1) "tax"]
      tokens "date" `shouldBe` [TKeyword (1, 1) "date"]
      tokens "emitter" `shouldBe` [TKeyword (1, 1) "emitter"]
      tokens "recipient" `shouldBe` [TKeyword (1, 1) "recipient"]
      tokens "note" `shouldBe` [TKeyword (1, 1) "note"]
      tokens "condition" `shouldBe` [TKeyword (1, 1) "condition"]

    it "should report identifiers" $ do
      tokens "myVar" `shouldBe` [TIdentifier (1, 1) "myVar"]
      tokens "my_var" `shouldBe` [TIdentifier (1, 1) "my_var"]
      tokens "my2var" `shouldBe` [TIdentifier (1, 1) "my2var"]
