module Scripting.Internal.ParserSpec where

import Scripting.Internal.Lexer
import Scripting.Internal.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "should recognize arithmetic expressions" $ do
      parse
        [ TNumber (1, 1) "1",
          TSymbol (1, 2) "+",
          TNumber (1, 3) "2"
        ]
        `shouldBe` [Sum (LiteralNumber 1) (LiteralNumber 2)]
      parse
        [ TNumber (1, 1) "1",
          TSymbol (1, 2) "-",
          TNumber (1, 3) "2"
        ]
        `shouldBe` [Substract (LiteralNumber 1) (LiteralNumber 2)]
      parse
        [ TNumber (1, 1) "1",
          TSymbol (1, 2) "*",
          TNumber (1, 3) "2"
        ]
        `shouldBe` [Multiply (LiteralNumber 1) (LiteralNumber 2)]
      parse
        [ TNumber (1, 1) "1",
          TSymbol (1, 2) "/",
          TNumber (1, 3) "2"
        ]
        `shouldBe` [Divide (LiteralNumber 1) (LiteralNumber 2)]

    it "should apply standard precedence for arithmetic operations" $ do
      parse -- 1 + 2 - 3 * (4 + -5) / 6
        [ TNumber (1, 1) "1",
          TSymbol (1, 2) "+",
          TNumber (1, 3) "2",
          TSymbol (1, 4) "-",
          TNumber (1, 5) "3",
          TSymbol (1, 6) "*",
          TSymbol (1, 7) "(",
          TNumber (1, 8) "4",
          TSymbol (1, 9) "+",
          TSymbol (1, 10) "-",
          TNumber (1, 11) "5",
          TSymbol (1, 12) ")",
          TSymbol (1, 13) "/",
          TNumber (1, 14) "6"
        ]
        `shouldBe` [ Substract
                       ( Sum
                           (LiteralNumber 1)
                           (LiteralNumber 2)
                       )
                       ( Divide
                           ( Multiply
                               (LiteralNumber 3)
                               (Sum (LiteralNumber 4) (Negate (LiteralNumber 5)))
                           )
                           (LiteralNumber 6)
                       )
                   ]

    it "should recognize assignments" $ do
      parse
        [ TIdentifier (1, 1) "a",
          TSymbol (1, 2) "=",
          TNumber (1, 3) "1",
          TSymbol (1, 4) "+",
          TNumber (1, 5) "2"
        ]
        `shouldBe` [ Assignment
                       "a"
                       ( Sum
                           (LiteralNumber 1)
                           (LiteralNumber 2)
                       )
                   ]

    it "should recognize multiple statements" $ do
      parse
        [ TIdentifier (1, 1) "a",
          TSymbol (1, 2) "=",
          TNumber (1, 3) "1",
          TNumber (2, 3) "1",
          TSymbol (2, 4) "+",
          TNumber (2, 5) "2"
        ]
        `shouldBe` [ Assignment "a" (LiteralNumber 1),
                     Sum (LiteralNumber 1) (LiteralNumber 2)
                   ]
