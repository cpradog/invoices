{
{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-|
Description: Script language tokenizer
Stability: experimental
-}
module Scripting.Internal.Lexer (
    tokens,
    Token (..),
    TokenLocation,
    tokenLocation,
) where
}
%wrapper "posn"

$digit      = [0-9]
$numsep     = [_\ ]
$symbol     = [ \+ \- \* \/ \= \( \) \[ \] \{ \} ]
$identS     = [a-zA-Z_]
$identC     = [a-zA-Z0-9_]

@nonwhite   = [^$white]
@number     = $digit ($digit | $numsep)* (\. ($digit | $numsep)+)?
@quotedStr  = \" ( \\. | [^\"\\] )* \"
@identifier = $identS $identC*
@keyword    = (
            -- DSL keywords
               reference | tax | date | emitter | recipient | note | condition
            )

tokens :-

@number               { \p s -> TNumber     (loc p) (stripNumber s) }
@quotedStr            { \p s -> TString     (loc p) (stripString s) }
$symbol               { \p s -> TSymbol     (loc p) s }
@keyword              { \p s -> TKeyword    (loc p) s }
@identifier           { \p s -> TIdentifier (loc p) s }

"#".*                 ;
$white                ;
@nonwhite+            { \p s -> TUnknown (loc p) s }

{

-- | The token row and column pair.
type TokenLocation = (Int, Int)

-- | Convert AlexPosn into a row and column pair.
loc :: AlexPosn -> TokenLocation
loc (AlexPn a y x) = (y, x)

-- | Represents a token
data Token
    = -- | A non recognized token
      TUnknown    TokenLocation String
    | -- | A literal number
      TNumber     TokenLocation String
    | -- | A valid syntax symbol
      TSymbol     TokenLocation String
    | -- | A keyword
      TKeyword    TokenLocation String
    | -- | An identifier
      TIdentifier TokenLocation String
    | -- | A literal string
      TString     TokenLocation String
    deriving (Show, Eq)

-- | Location of a token in an script string
tokenLocation :: Token -> TokenLocation
tokenLocation (TUnknown p _) = p
tokenLocation (TNumber  p _) = p

-- | tokenize given string
tokens :: String -> [Token]
tokens = alexScanTokens

-- | remove number separators from final string
stripNumber :: String -> String
stripNumber s = [ c | c <- s, not (c `elem` " _") ]

-- | remove string quotes
stripString :: String -> String
stripString ('\"' : s) = reverse (stripString (reverse s))
stripString s = s
}
