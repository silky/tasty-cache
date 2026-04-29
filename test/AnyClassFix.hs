{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
-- | DeriveAnyClass fixture.
--
-- 'AnyClassThing' gets its 'Greeter' instance from @deriving anyclass@,
-- whose method bodies are precisely the class defaults.
module AnyClassFix (AnyClassThing (..), Greeter (..), greetAnyClass) where

class Greeter a where
  hello :: a -> String
  hello _ = "anyclass-greeting"

data AnyClassThing = AnyClassThing
  deriving anyclass Greeter

greetAnyClass :: String
greetAnyClass = hello AnyClassThing
