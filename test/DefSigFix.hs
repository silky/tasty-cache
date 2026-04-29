{-# LANGUAGE DefaultSignatures #-}
-- | DefaultSignatures fixture.
--
-- 'DefSigUser' has no explicit method override; @describe@ is supplied
-- by the class default, whose body lives in this module.  A test that
-- calls @describe (DefSigUser n)@ must invalidate when the default body
-- is edited, even though the call site never names @describe@'s default
-- definition.
module DefSigFix (DefSigClass (..), DefSigUser (..), defSigResult) where

class DefSigClass a where
  describe :: a -> String
  default describe :: Show a => a -> String
  describe x = "defsig-prefix " ++ show x

newtype DefSigUser = DefSigUser Int deriving (Show)

instance DefSigClass DefSigUser

defSigResult :: String
defSigResult = describe (DefSigUser 42)
