{-# LANGUAGE CPP #-}
-- | C-preprocessor scenario.
--
-- The HIE file stores the *original* (pre-CPP) source bytes, so
-- @hie_hs_src@ contains the #define directives literally.  However, the
-- declaration span for 'scaled' covers only the lines where 'scaled' is
-- written — not the #define line above it.  Changing SCALE_FACTOR therefore
-- changes a *different* line than the one hashed for 'scaled', so the dep
-- hash does NOT change and the test stays cached even though the behaviour
-- changed.  This is a documented limitation for CPP-defined constants.
--
-- By contrast, if you edit the body of 'scaled' or 'cppVersion' directly,
-- the dep hash changes and the test re-runs normally.
module CPPDemo (scaled, cppVersion) where

#define SCALE_FACTOR 3
#define CPP_VERSION "v3"

scaled :: Int -> Int
scaled n = n * SCALE_FACTOR

cppVersion :: String
cppVersion = CPP_VERSION
