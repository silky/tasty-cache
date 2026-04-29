{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
-- | OverloadedLabels fixture.
--
-- The label literal @#k@ in 'sampleOvLabel' desugars to
-- @fromLabel \@\"k\" \@Lbl@ via 'IsLabel'.
module OvLabFix (Lbl(..), sampleOvLabel) where

import           GHC.OverloadedLabels (IsLabel (..))

newtype Lbl = Lbl String deriving (Eq, Show)

instance IsLabel "k" Lbl where
  fromLabel = Lbl "ovlabel-k"

sampleOvLabel :: Lbl
sampleOvLabel = #k
