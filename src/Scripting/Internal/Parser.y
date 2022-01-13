{
{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-|
Description: Script language parser
Stability: experimental
-}
module Scripting.Internal.Parser (
    parse,
    parseError,
    ASTNode (..)
) where

import Scripting.Internal.Lexer
import Data.Decimal
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    num       { TNumber      p $$  }
    str       { TString      p $$  }
    ident     { TIdentifier  p $$  }
    '='       { TSymbol      p "=" }
    '+'       { TSymbol      p "+" }
    '-'       { TSymbol      p "-" }
    '*'       { TSymbol      p "*" }
    '/'       { TSymbol      p "/" }
    '('       { TSymbol      p "(" }
    ')'       { TSymbol      p ")" }
    '{'       { TSymbol      p "{" }
    '}'       { TSymbol      p "}" }
    '['       { TSymbol      p "[" }
    ']'       { TSymbol      p "]" }

%left '+' '-'
%left '*' '/'
%left NEG

%%

stmts: stmt stmts                 { $1:$2 }
     | stmt                       { [$1] }

stmt: expr                        { $1 }
    | assign                      { $1 }

expr: expr '+' expr               { Sum $1 $3 }
    | expr '-' expr               { Substract $1 $3 }
    | expr '*' expr               { Multiply $1 $3 }
    | expr '/' expr               { Divide $1 $3 }
    | '(' expr ')'                { $2 }
    | '-' expr %prec NEG          { Negate $2 }
    | num                         { LiteralNumber (read $1) }
    | str                         { LiteralString $1 }
    | ident                       { VariableRef $1 }

assign: ident '=' expr            { Assignment $1 $3 }

{

-- | Represents an abstract syntax tree node
data ASTNode
        = -- | Literal number
          LiteralNumber Decimal
        | -- | Literal string
          LiteralString String
        | -- | Variable reference
          VariableRef String
        | -- | Arithmetic sum
          Sum ASTNode ASTNode
        | -- | Arithmetic substraction
          Substract ASTNode ASTNode
        | -- | Arithmetic multiplication
          Multiply ASTNode ASTNode
        | -- | Arithmetic division
          Divide ASTNode ASTNode
        | -- | Invert expression sign
          Negate ASTNode
        | -- | Variable assignment
          Assignment String ASTNode
        deriving (Show, Eq)

-- | Generate the parse error description
parseError :: [Token] -> a
parseError _ = error "TODO: explain the error"

-- | Parse given tokens into an AST
parse :: [Token] -> [ASTNode]
}
