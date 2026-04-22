{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell code generators used by Arithmetic.hs.
module Meta (adderExpr, doublerExpr) where

import           Language.Haskell.TH

-- | Generates the expression @\x -> x + n@.
adderExpr :: Int -> Q Exp
adderExpr n = [| \x -> x + $(litE (integerL (fromIntegral n))) |]

-- | Generates the expression @\x -> x * n@.
doublerExpr :: Int -> Q Exp
doublerExpr n = [| \x -> x * $(litE (integerL (fromIntegral n))) |]
