{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
-- | DerivingVia fixture.
--
-- 'ViaThing' gets its 'Speaker' instance via the @Loud@ newtype, whose
-- @shout@ body lives elsewhere in this module.
module ViaFix (Loud (..), Speaker (..), ViaThing (..), viaResult) where

class Speaker a where
  shout :: a -> String

newtype Loud a = Loud a

instance Show a => Speaker (Loud a) where
  shout (Loud x) = "via-loud-prefix " ++ show x

newtype ViaThing = ViaThing Int
  deriving stock Show
  deriving Speaker via (Loud Int)

viaResult :: String
viaResult = shout (ViaThing 7)
