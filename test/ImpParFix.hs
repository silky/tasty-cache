{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
-- | ImplicitParams fixture.
--
-- 'greetImpl' depends on the implicit parameter @?greeting@.  The value
-- is bound at the call site in 'impParResult'; mutating its bytes tests
-- whether the BFS catches changes to implicit-param values.
module ImpParFix (greetImpl, impParResult) where

greetImpl :: (?greeting :: String) => String -> String
greetImpl name = ?greeting ++ " " ++ name

impParResult :: String
impParResult = let ?greeting = "imppar-hello" in greetImpl "world"
