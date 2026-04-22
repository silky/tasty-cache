{-# LANGUAGE GADTs #-}
-- | GADT scenario: a type-safe expression language.
--
-- eval and pretty are completely independent functions over the same GADT.
-- Editing eval does NOT invalidate pretty tests, and vice versa, because
-- they occupy distinct source spans and neither is a dependency of the other
-- in the HIE identifier graph.
module Expr (Expr(..), eval, pretty) where

data Expr a where
  Lit     :: Int  -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add     :: Expr Int  -> Expr Int  -> Expr Int
  Eq      :: Expr Int  -> Expr Int  -> Expr Bool
  If      :: Expr Bool -> Expr a    -> Expr a    -> Expr a

-- | Evaluate an expression to its Haskell value.
eval :: Expr a -> a
eval (Lit n)     = n
eval (BoolLit b) = b
eval (Add x y)   = eval x + eval y
eval (Eq  x y)   = eval x == eval y
eval (If c t f)  = if eval c then eval t else eval f

-- | Pretty-print an expression.
pretty :: Expr a -> String
pretty (Lit n)     = show n
pretty (BoolLit b) = show b
pretty (Add x y)   = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
pretty (Eq  x y)   = "(" ++ pretty x ++ " == " ++ pretty y ++ ")"
pretty (If c t f)  = "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty f
