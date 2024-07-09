{
module LexString (Token(..), lexer) where
}

%wrapper "basic"

-- Predefined character classes

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- Symbols and non-identifier-like reserved words

@rsyms = \+ | \* | \( | \)

tokens :-

  $white+                       ;
  \"alpha+\"        { \s -> TokenString (head (init s))}
  \(				{ \s -> OpenPar }
  \)				{ \s -> ClosedPar }
  \+				{ \s -> TokenPlus }
  \*				{ \s -> TokenTimes }

{
data Token
  = OpenPar
  | ClosedPar
  | TokenPlus
  | TokenTimes
  | TokenInt Int
  | TokenString String
  deriving (Eq, Show)

lexer = alexScanTokens
}
