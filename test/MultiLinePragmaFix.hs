{-# LANGUAGE
    LambdaCase
    , ScopedTypeVariables
    #-}
-- | Multi-line pragma fixture.
--
-- The @{-# LANGUAGE #-}@ block spans multiple lines.  Only the first
-- line (starting with @{-#@) is captured by the pragma-line filter in
-- 'transitiveDeps'; the continuation lines are missed.  This is the
-- documented limitation we pin a regression test on.
module MultiLinePragmaFix (mlpResult) where

mlpResult :: Int -> String
mlpResult = \case
  0 -> "mlp-zero"
  _ -> "mlp-nonzero"
