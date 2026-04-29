-- | Assertion helpers for fingerprint-mutation tests.
--
-- Each helper loads the project's HIE files, applies an in-memory
-- substitution to one fixture's source bytes (without recompiling), and
-- asserts that the fingerprint of a named test changed (or did not).
--
-- This is the same machinery used by 'FalseNegatives'; extracted so that
-- 'ExtensionCoverage' can reuse it without circular imports.
module FixtureHelpers
  ( fingerprintBeforeAfter
  , assertFingerprintChanged
  , assertFingerprintUnchanged
  ) where

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BSC
import qualified Data.Map.Strict              as Map
import           GHC.Fingerprint              (Fingerprint (..))
import           Test.Tasty.HUnit

import           Test.Tasty.HieCache          (internalComputeFingerprint)

-- | Compute fingerprints for @leafName@ before and after applying the
-- given source-byte substitution to @hieFile@'s in-memory copy of
-- @hsFile@.
fingerprintBeforeAfter
  :: String       -- ^ leaf name of the test
  -> FilePath     -- ^ on-disk source file (read for the original bytes)
  -> FilePath     -- ^ HIE filename to override (key into the overrides map)
  -> String       -- ^ needle: substring to replace
  -> String       -- ^ replacement
  -> IO (Maybe Fingerprint, Maybe Fingerprint)
fingerprintBeforeAfter leafName hsFile hieFile needle replacem = do
  let cabalHash = Fingerprint 0 0
      hieDir    = ".hie"
      needleBs  = BSC.pack needle
      replBs    = BSC.pack replacem
  origBytes <- BS.readFile hsFile
  let mutated = case BSC.breakSubstring needleBs origBytes of
        (before, rest)
          | BS.null rest -> error
              ("fixture broken: needle '" ++ needle ++
               "' not in " ++ hsFile)
          | otherwise ->
              before <> replBs <> BS.drop (BS.length needleBs) rest
      overrides = Map.singleton hieFile mutated
  fp1 <- internalComputeFingerprint hieDir Map.empty cabalHash leafName
  fp2 <- internalComputeFingerprint hieDir overrides cabalHash leafName
  return (fp1, fp2)

-- | Assert that mutating the given source needle changes the
-- fingerprint of the named test.
assertFingerprintChanged
  :: String -> FilePath -> FilePath -> String -> String -> Assertion
assertFingerprintChanged leafName hsFile hieFile needle replacem = do
  (fp1, fp2) <- fingerprintBeforeAfter leafName hsFile hieFile needle replacem
  case (fp1, fp2) of
    (Just a, Just b) ->
      assertBool
        ("fingerprint of \"" ++ leafName ++
         "\" should change after editing '" ++ needle ++
         "' but got " ++ show a ++ " == " ++ show b)
        (a /= b)
    _ ->
      assertFailure
        ("could not compute fingerprint for \"" ++ leafName ++
         "\": fp1=" ++ show fp1 ++ ", fp2=" ++ show fp2)

-- | Assert that mutating the given source needle does /not/ change
-- the fingerprint of the named test.
assertFingerprintUnchanged
  :: String -> FilePath -> FilePath -> String -> String -> Assertion
assertFingerprintUnchanged leafName hsFile hieFile needle replacem = do
  (fp1, fp2) <- fingerprintBeforeAfter leafName hsFile hieFile needle replacem
  case (fp1, fp2) of
    (Just a, Just b) ->
      assertBool
        ("fingerprint of \"" ++ leafName ++
         "\" should NOT change after editing '" ++ needle ++
         "' but got " ++ show a ++ " /= " ++ show b)
        (a == b)
    _ ->
      assertFailure
        ("could not compute fingerprint for \"" ++ leafName ++
         "\": fp1=" ++ show fp1 ++ ", fp2=" ++ show fp2)
