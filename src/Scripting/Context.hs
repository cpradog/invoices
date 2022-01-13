{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}

-- |
--  Description: Script execution context
--  Stability: experimental
--
--  The execution context stores the result state during script execution and can be
--  interrogated after execution to get results.
module Scripting.Context
  ( -- * Values
    Value (..),
    valueAsDecimal,
    valueAsString,

    -- * Script evaluation
    ScriptContext (..),
    emptyContext,
    variable,
    setVariable,
    evaluate,
  )
where

import Control.Error.Safe
import Control.Error.Util (fmapR)
import Control.Lens
import Control.Monad.State
import Data.Decimal
import Data.Either.Combinators
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Internal.Fusion.Types (Scan (Scan1))
import Scripting.Internal.Lexer
import Scripting.Internal.Parser

-- | Represents a variant value
data Value
  = -- | A decimal value
    NumberValue Decimal
  | -- | An string value
    StringValue String
  deriving (Show, Eq)

-- | Returns the string representation of the value.
valueAsString :: Maybe Value -> Maybe String
valueAsString Nothing = Nothing
valueAsString (Just (NumberValue v)) = Just (show v)
valueAsString (Just (StringValue v)) = Just v

-- | Converts, if needed, the value into a decimal number.
valueAsDecimal :: Maybe Value -> Maybe Decimal
valueAsDecimal Nothing = Nothing
valueAsDecimal (Just (NumberValue v)) = Just v
valueAsDecimal (Just (StringValue v)) = Nothing

-- -- | The script execution context
data ScriptContext = ScriptContext
  { -- | The resulted variables
    _variables :: Map String Value,
    -- | The registered taxes
    _taxes :: Map String String
  }
  deriving (Show, Eq)

makeLenses ''ScriptContext

-- | An empty initial context
emptyContext :: ScriptContext
emptyContext = ScriptContext Map.empty Map.empty

-- | Maybe the variable value
variable :: String -> ScriptContext -> Maybe Value
variable name ctx = ctx ^. variables . at name

-- | Maybe change context variable value
setVariable :: String -> Maybe Value -> ScriptContext -> ScriptContext
setVariable name value = (variables . at name) .~ value

-- | Evaluate given script and return final context
evaluate :: String -> ScriptContext
evaluate script = do
  evalState (eval ast) ctx
  where
    ast = parse (tokens script)
    ctx = emptyContext
    eval (n : ns) = do
      evaluateNode n
      eval ns
    eval [] = get

-- | Evaluate single AST branch
evaluateNode :: ASTNode -> State ScriptContext (Either String Value)
evaluateNode (LiteralNumber d) = do return (Right (NumberValue d))
evaluateNode (LiteralString s) = do return (Right (StringValue s))
evaluateNode (VariableRef name) = gets $ justErr ("Variable '" ++ name ++ "' not defined") . variable name
evaluateNode (Sum n1 n2) = evalExpression (\d1 d2 -> Just (d1 + d2)) n1 n2
evaluateNode (Substract n1 n2) = evalExpression (\d1 d2 -> Just (d1 - d2)) n1 n2
evaluateNode (Multiply n1 n2) = evalExpression (\d1 d2 -> Just (d1 * d2)) n1 n2
evaluateNode (Divide n1 n2) = evalExpression (\d1 d2 -> Just (d1 / d2)) n1 n2
evaluateNode (Negate n) = do
  value <- evaluateNode n
  return
    ( case value of
        Left e -> Left e
        Right v ->
          maybeToRight
            "not a number"
            (fmap (NumberValue . negate) (valueAsDecimal (Just v)))
    )
evaluateNode (Assignment name n) = do
  ctx <- get
  value <- evaluateNode n
  whenRight value (\v -> put (setVariable name (Just v) ctx))
  return value

-- | Evaluate expression
evalExpression :: (Decimal -> Decimal -> Maybe Decimal) -> ASTNode -> ASTNode -> State ScriptContext (Either String Value)
evalExpression op n1 n2 = do
  value1 <- fmapR (valueAsDecimal . Just) <$> evaluateNode n1
  value2 <- fmapR (valueAsDecimal . Just) <$> evaluateNode n2
  return
    ( case value2 of
        Left e -> Left e
        Right v1 -> case value2 of
          Left e -> Left e
          Right v2 -> eval v1 v2
    )
  where
    eval :: Maybe Decimal -> Maybe Decimal -> Either String Value
    eval Nothing _ = Left "not a number"
    eval _ Nothing = Left "not a number"
    eval (Just v1) (Just v2) = case result of
      Nothing -> Left "Not a number"
      Just r -> Right (NumberValue r)
      where
        result = op v1 v2
