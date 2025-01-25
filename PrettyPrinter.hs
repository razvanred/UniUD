module PrettyPrinter where

class PrettyPrinter a where
pp :: a -> String