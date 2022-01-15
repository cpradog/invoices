{-# LANGUAGE TemplateHaskell #-}
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
    taxes,
    variables,
    variable,
    setVariable,
    evaluate,
  )
where

import Control.Error (fmapR)
import Control.Error.Safe (justErr)
import Control.Lens
import Control.Monad.State
import Data.Decimal (Decimal, roundTo)
import Data.Either.Combinators (maybeToRight, unlessLeft, whenRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Internal.Debug (node)
import Data.Text.Internal.Fusion.Types (Scan (Scan1))
import Scripting.Internal.Lexer
import Scripting.Internal.Parser
import Text.Read (readMaybe)

-- | Represents a variant value
data Value
  = -- | A decimal value
    NumberValue Decimal
  | -- | An string value
    StringValue String
  deriving (Show, Eq)

-- | Returns the string representation of the value.
valueAsString :: Value -> Maybe String
valueAsString (NumberValue v) = Just (show v)
valueAsString (StringValue v) = Just v

-- | Converts, if needed, the value into a decimal number.
valueAsDecimal :: Value -> Maybe Decimal
valueAsDecimal (NumberValue v) = Just v
valueAsDecimal (StringValue v) = readMaybe v :: Maybe Decimal

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
setVariable name value = variables . at name .~ value

-- | Evaluate given script and return final context
evaluate :: String -> Either String ScriptContext
evaluate script = evalState (eval ast) emptyContext
  where
    ast = parse (tokens script)
    eval [] = gets Right
    eval (n : ns) = do
      value <- evalNode n
      case value of
        Left e -> return (Left e)
        Right _ -> eval ns

-- | Evaluate single AST branch
evalNode :: ASTNode -> State ScriptContext (Either String Value)
evalNode (LiteralNumber d) = return (Right (NumberValue d))
evalNode (LiteralString s) = return (Right (StringValue s))
evalNode (VariableRef name) = gets $ justErr ("Variable '" ++ name ++ "' not defined") . variable name
evalNode (Sum n1 n2) = evalExpr (+) n1 n2
evalNode (Substract n1 n2) = evalExpr (-) n1 n2
evalNode (Multiply n1 n2) = evalExpr (*) n1 n2
evalNode (Divide n1 n2) = evalExpr (/) n1 n2
evalNode (Negate n) = do
  value <- evalNode n
  return
    ( case value of
        Left e -> Left e
        Right v ->
          maybeToRight
            "not a number"
            (fmap (NumberValue . negate) (valueAsDecimal v))
    )
evalNode (Assignment name n) = do
  ctx <- get
  value <- evalNode n
  whenRight value (\v -> put (setVariable name (Just v) ctx))
  return value

-- | Evaluate an expression
evalExpr :: (Decimal -> Decimal -> Decimal) -> ASTNode -> ASTNode -> State ScriptContext (Either String Value)
evalExpr op n1 n2 = do
  value1 <- fmapR valueAsDecimal <$> evalNode n1
  value2 <- fmapR valueAsDecimal <$> evalNode n2
  return
    ( case value1 of
        Left e -> Left e
        Right v1 -> case value2 of
          Left e -> Left e
          Right v2 -> eval v1 v2
    )
  where
    eval :: Maybe Decimal -> Maybe Decimal -> Either String Value
    eval Nothing _ = Left "not a number"
    eval _ Nothing = Left "not a number"
    eval (Just v1) (Just v2) = Right (NumberValue (roundTo 5 (op v1 v2)))
