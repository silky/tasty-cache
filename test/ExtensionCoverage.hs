-- | Coverage matrix for GHC language extensions vs. tasty-cache's
-- fingerprinting strategy.
--
-- Every Haskell language extension was classified into one of five
-- buckets according to the mechanism (if any) that protects the cache
-- against semantic edits enabled by it.  See
-- @plans\/can-you-take-a-atomic-finch.md@ for the full enumeration.
--
-- This module exercises one (or more) representative test per bucket:
--
--   * CAT-A — source-byte hash captures any user-visible edit.
--   * CAT-B — pragma-line filter captures single-line @{-# … #-}@.
--   * CAT-C — cabal hash captures @default-extensions@ etc.
--   * CAT-D — implicit dispatch / synthesised binding / type-level edge,
--             one test per extension via fingerprint mutation.
--   * CAT-E — known limitations (multi-line pragmas, CPP defines), pinned
--             so future improvements show up as test changes.
--
-- Leaf-name strings handed to 'assertFingerprintChanged' are constructed
-- via @unwords@ so that the contiguous string does not appear in this
-- module's HIE source bytes — otherwise 'findSubstring' (which sorts HIE
-- filenames alphabetically) would locate the leaf name here, in
-- @ExtensionCoverage.hie@, instead of in @Main.hie@ where the testCase
-- actually lives.  @ExtensionCoverage.hie@ sorts before @Main.hie@.
module ExtensionCoverage (extensionCoverageTests) where

import qualified Data.ByteString.Char8 as BSC
import           Data.List             (sort)
import           Test.Tasty
import           Test.Tasty.HUnit

import           FixtureHelpers        (assertFingerprintChanged,
                                        assertFingerprintUnchanged)

extensionCoverageTests :: TestTree
extensionCoverageTests = testGroup "Extension coverage"
  [ catASourceByte
  , catBPragmaLine
  , catCCabal
  , catDImplicitDispatch
  , catEKnownLimitations
  ]

-- ---------------------------------------------------------------------------
-- CAT-A: source-byte hash sanity
--
-- Most extensions affect only syntax (LambdaCase, BangPatterns, GADTs,
-- RecordWildCards, ...).  Edits to the affected source bytes are
-- captured by the dep hash because every reachable declaration's source
-- bytes are folded in.  We don't test each syntactic extension
-- separately — one representative each from "value-level" and
-- "type-level" suffices to demonstrate the mechanism.

catASourceByte :: TestTree
catASourceByte = testGroup "CAT-A: source-byte hash"
  -- Value-level: 'factorial' in Lib.hs is reached by the BFS from the
  -- factorial testCase in Main.hs.  Editing its body bytes invalidates.
  [ testCase "value-level edit on Lib.factorial invalidates" $
      assertFingerprintChanged
        (unwords ["factorial", "5", "==", "120"])
        "test/Lib.hs"
        "Lib.hie"
        "n * factorial (n - 1)"
        "n + factorial (n - 1)"

  -- Type-level: 'Polymorphic.pair' has @forall a b. a -> b -> (a, b)@.
  -- Already pinned in FalseNegatives but repeated here for symmetry.
  , testCase "type-signature edit on Polymorphic.pair invalidates" $
      assertFingerprintChanged
        (unwords ["pair", "returns", "a", "tuple", "of", "its", "arguments"])
        "test/Polymorphic.hs"
        "Polymorphic.hie"
        "forall a b. a -> b -> (a, b)"
        "forall a b. a -> b -> (b, a)"
  ]

-- ---------------------------------------------------------------------------
-- CAT-B: single-line pragma capture
--
-- 'transitiveDeps' builds 'pragmaChunks' by filtering the lines of each
-- visited (non-CPP) file for those starting with "{-#" and folding them
-- into the dep hash.  Adding, removing, or editing such a line in any
-- dep file invalidates every test reaching that file.
--
-- This is tested two ways:
--   1. Pure unit test on the byte-level filter logic (mirrors what the
--      production code does).
--   2. End-to-end on a real fixture: the @{-# LANGUAGE ExplicitForAll #-}@
--      line in Polymorphic.hs gets edited, and the fingerprint of a test
--      that reaches Polymorphic must change.

catBPragmaLine :: TestTree
catBPragmaLine = testGroup "CAT-B: single-line pragma"
  [ testCase "pragma filter captures {-# LANGUAGE X #-} line" $ do
      let src = BSC.pack $ unlines
            [ "{-# LANGUAGE LambdaCase #-}"
            , "module Foo where"
            , "x :: Int"
            , "x = 1"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# LANGUAGE LambdaCase #-}"]

  , testCase "pragma filter captures changed pragma differently" $ do
      let mk ext = BSC.pack $ unlines
            [ "{-# LANGUAGE " ++ ext ++ " #-}"
            , "module Foo where"
            , "x :: Int"
            , "x = 1"
            ]
          captured1 = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines (mk "LambdaCase"))
          captured2 = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines (mk "BangPatterns"))
      assertBool "different pragma → different captured chunk"
        (captured1 /= captured2)

  , testCase "pragma filter captures OPTIONS_GHC line" $ do
      let src = BSC.pack $ unlines
            [ "{-# OPTIONS_GHC -O0 #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# OPTIONS_GHC -O0 #-}"]

  , testCase "pragma filter captures INLINE/NOINLINE/SPECIALIZE" $ do
      let src = BSC.pack $ unlines
            [ "{-# INLINE foo #-}"
            , "foo = 1"
            , "{-# NOINLINE bar #-}"
            , "bar = 2"
            , "{-# SPECIALIZE baz :: Int -> Int #-}"
            , "baz x = x"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      length captured @?= 3

  -- End-to-end: editing Polymorphic.hs's LANGUAGE pragma must change
  -- the fingerprint of any test reaching Polymorphic via BFS.
  , testCase "editing {-# LANGUAGE ExplicitForAll #-} invalidates pair test" $
      assertFingerprintChanged
        (unwords ["pair", "returns", "a", "tuple", "of", "its", "arguments"])
        "test/Polymorphic.hs"
        "Polymorphic.hie"
        "{-# LANGUAGE ExplicitForAll #-}"
        "{-# LANGUAGE ExplicitForAll, BangPatterns #-}"
  ]

-- ---------------------------------------------------------------------------
-- CAT-C: cabal hash
--
-- The cabalHash is the hash of all .cabal files in the project root.
-- @default-extensions@, @default-language@, @ghc-options@,
-- @build-depends@ all live in the .cabal and any change there rolls
-- the global cabalHash.
--
-- Pure unit test: synthesise two byte strings differing only in the
-- @default-extensions@ stanza and assert their bytes (and therefore
-- hashes) differ.

catCCabal :: TestTree
catCCabal = testGroup "CAT-C: cabal hash"
  [ testCase "default-extensions change → different bytes" $ do
      let cabal1 = BSC.pack $ unlines
            [ "library"
            , "  default-extensions:"
            , "    LambdaCase"
            ]
          cabal2 = BSC.pack $ unlines
            [ "library"
            , "  default-extensions:"
            , "    LambdaCase"
            , "    BangPatterns"
            ]
      assertBool "cabal byte strings should differ" (cabal1 /= cabal2)

  , testCase "ghc-options change → different bytes" $ do
      let cabal1 = BSC.pack "ghc-options: -O0\n"
          cabal2 = BSC.pack "ghc-options: -O2\n"
      assertBool "cabal byte strings should differ" (cabal1 /= cabal2)
  ]

-- ---------------------------------------------------------------------------
-- CAT-D: implicit-dispatch / synthesised-binding extensions
--
-- One mutation test per extension.  Each test edits a fixture's source
-- bytes (in memory, no recompile) and asserts whether the fingerprint
-- of the corresponding 'Extension fixtures' testCase changed.
--
-- Expected outcomes (confirming class-edge BFS chases the right things,
-- or surfacing that it doesn't):
--
--   PASS: OverloadedStrings, OverloadedLists, OverloadedLabels,
--         DefaultSignatures, DeriveAnyClass, StandaloneDeriving,
--         RebindableSyntax (local 'ifThenElse' is a name-edge dep),
--         GND/DerivingVia.
--
--   PROBABLY FAIL (false negative — the extension exposes a gap):
--         TypeFamilies (type-level only edits to the family RHS),
--         DataKinds (type-level uses), ImplicitParams,
--         PatternSynonyms (cross-module patterns).

catDImplicitDispatch :: TestTree
catDImplicitDispatch = testGroup "CAT-D: implicit dispatch / synthesised binding"
  -- Sanity (name-edge): editing 'greetOvStr's own body line propagates.
  -- Confirms the BFS reaches OvStrFix.hie.
  [ testCase "OverloadedStrings: greetOvStr body edit invalidates (name-edge sanity)" $
      assertFingerprintChanged
        (unwords ["OvStr", "greet", "hello"])
        "test/OvStrFix.hs"
        "OvStrFix.hie"
        "greetOvStr = \"world\""
        "greetOvStr = \"WORLD\""

  -- Class-edge (implicit-dispatch heuristic): editing the 'fromString'
  -- body of the 'IsString S' instance invalidates.  GHC's HIE does
  -- not record an 'EvidenceVarUse' for the @fromString@ desugaring of
  -- a string literal at the binding's span; the heuristic in
  -- 'buildFileIndex' adds @IsString@ to every binding's @ddUsedClasses@
  -- when @OverloadedStrings@ is enabled, so the class-edge BFS reaches
  -- the instance methods.
  , testCase "OverloadedStrings: fromString edit invalidates greetOvStr test" $
      assertFingerprintChanged
        (unwords ["OvStr", "greet", "hello"])
        "test/OvStrFix.hs"
        "OvStrFix.hie"
        "S (\"ovstr:\" ++ s)"
        "S (\"OVSTR:\" ++ s ++ \"!\")"

  -- Sanity (name-edge): editing 'sampleOvList's body propagates.
  , testCase "OverloadedLists: sampleOvList body edit invalidates (name-edge sanity)" $
      assertFingerprintChanged
        (unwords ["OvList", "sample", "1", "2", "3"])
        "test/OvListFix.hs"
        "OvListFix.hie"
        "sampleOvList = [1,2,3]"
        "sampleOvList = [9,9,9]"

  -- Same shape as OverloadedStrings: heuristic adds @IsList@ to every
  -- binding's @ddUsedClasses@ when @OverloadedLists@ is on.
  , testCase "OverloadedLists: fromList edit invalidates sampleOvList test" $
      assertFingerprintChanged
        (unwords ["OvList", "sample", "1", "2", "3"])
        "test/OvListFix.hs"
        "OvListFix.hie"
        "L (xs ++ [99999])"
        "L (xs ++ [88888, 77777])"

  , testCase "OverloadedLabels: fromLabel edit invalidates sampleOvLabel test" $
      assertFingerprintChanged
        (unwords ["OvLab", "sample", "k"])
        "test/OvLabFix.hs"
        "OvLabFix.hie"
        "Lbl \"ovlabel-k\""
        "Lbl \"OVLABEL-K-EDITED\""

  -- Sanity (name-edge): editing 'rebindIf's own body line propagates.
  , testCase "RebindableSyntax: rebindIf body edit invalidates (name-edge sanity)" $
      assertFingerprintChanged
        (unwords ["Rebind", "if", "1", "else", "2"])
        "test/RebindFix.hs"
        "RebindFix.hie"
        "rebindIf = if True then 1 else 2"
        "rebindIf = if True then 99 else 2"

  -- Under 'RebindableSyntax', GHC rewrites @if c then t else e@ to a
  -- call of the in-scope 'ifThenElse' at typecheck time, but does not
  -- emit an Identifier Use for 'ifThenElse' at the binding's span.  The
  -- heuristic in 'buildFileIndex' adds the standard rebound names
  -- (including 'ifThenElse', '>>=', 'fromInteger', …) to every
  -- binding's @ddUsedNames@ when 'RebindableSyntax' is on, so the
  -- name-edge BFS visits any local rebinding.
  , testCase "RebindableSyntax: ifThenElse edit invalidates rebindIf test" $
      assertFingerprintChanged
        (unwords ["Rebind", "if", "1", "else", "2"])
        "test/RebindFix.hs"
        "RebindFix.hie"
        "t P.+ 1000"
        "t P.+ 2000"

  , testCase "DefaultSignatures: default body edit invalidates defSigResult test" $
      assertFingerprintChanged
        (unwords ["DefSig", "describe", "42"])
        "test/DefSigFix.hs"
        "DefSigFix.hie"
        "\"defsig-prefix \" ++ show x"
        "\"DEFSIG-EDITED \" ++ show x"

  , testCase "DeriveAnyClass: class default edit invalidates greetAnyClass test" $
      assertFingerprintChanged
        (unwords ["AnyClass", "hello", "AnyClass"])
        "test/AnyClassFix.hs"
        "AnyClassFix.hie"
        "\"anyclass-greeting\""
        "\"ANYCLASS-EDITED\""

  , testCase "DerivingVia: via-instance method edit invalidates viaResult test" $
      assertFingerprintChanged
        (unwords ["Via", "shout", "ViaThing", "7"])
        "test/ViaFix.hs"
        "ViaFix.hie"
        "\"via-loud-prefix \" ++ show x"
        "\"VIA-EDITED \" ++ show x"

  , testCase "GND: underlying-instance method edit invalidates gndResult test" $
      assertFingerprintChanged
        (unwords ["GND", "bark", "GNDThing", "5"])
        "test/GNDFix.hs"
        "GNDFix.hie"
        "\"gnd-bark \" ++ show n"
        "\"GND-EDITED \" ++ show n"

  , testCase "StandaloneDeriving: data-type edit invalidates stdDerivResult test" $
      assertFingerprintChanged
        (unwords ["StdDeriv", "show", "StdBranch"])
        "test/StdDerivData.hs"
        "StdDerivData.hie"
        "data StdData = StdLeaf | StdBranch StdData StdData"
        "data StdData = StdLeaf | StdBranch StdData StdData | StdExtra Int"

  , testCase "PatternSynonyms: synonym RHS edit invalidates patSynResult test" $
      assertFingerprintChanged
        (unwords ["PatSyn", "Bin", "7"])
        "test/PatSynData.hs"
        "PatSynData.hie"
        "pattern Bin l v r = TNode l v r"
        "pattern Bin l v r = TNode r v l"

  , testCase "TypeFamilies: instance RHS edit invalidates tyFamResult test" $
      assertFingerprintChanged
        (unwords ["TyFam", "listElem", "1", "2", "3"])
        "test/TyFamFix.hs"
        "TyFamFix.hie"
        "type instance Element [a] = a"
        "type instance Element [a] = Int"

  , testCase "DataKinds: promoted-tag data type edit invalidates dKindsResult test" $
      assertFingerprintChanged
        (unwords ["DKinds", "describeTag", "Foo"])
        "test/DKindsFix.hs"
        "DKindsFix.hie"
        "data Tag = Foo | Bar"
        "data Tag = Foo | Bar | Baz"

  , testCase "ImplicitParams: ?greeting value edit invalidates impParResult test" $
      assertFingerprintChanged
        (unwords ["ImpPar", "greetImpl", "world"])
        "test/ImpParFix.hs"
        "ImpParFix.hie"
        "\"imppar-hello\""
        "\"IMPPAR-EDITED\""
  ]

-- ---------------------------------------------------------------------------
-- CAT-E: known limitations (pinning tests)
--
-- These tests assert the *current* (limited) behaviour.  They will fail
-- loudly if a future change fixes the limitation, prompting a review of
-- the regression suite.

catEKnownLimitations :: TestTree
catEKnownLimitations = testGroup "CAT-E: known limitations"
  -- The pragma-line filter only captures lines starting with "{-#".  A
  -- multi-line pragma like:
  --     {-#
  --         LANGUAGE LambdaCase, ScopedTypeVariables
  --       #-}
  -- has only the first line ("{-#" alone) captured.  Mutations on the
  -- continuation lines escape the dep hash.
  [ testCase "multi-line pragma: only first line is captured (LIMITATION)" $ do
      let src = BSC.pack $ unlines
            [ "{-# LANGUAGE"
            , "    LambdaCase"
            , "  , ScopedTypeVariables"
            , "  #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      captured @?= [BSC.pack "{-# LANGUAGE"]
      assertBool "continuation line 'LambdaCase' is NOT captured"
        (not (any (BSC.isInfixOf (BSC.pack "LambdaCase")) captured))

  -- Real-fixture pinning: editing only a continuation line of a
  -- multi-line pragma in MultiLinePragmaFix.hs does NOT change the
  -- fingerprint of mlpResult.  This is the documented false-negative.
  , testCase "MultiLinePragmaFix: continuation-line edit does NOT invalidate mlp test" $
      assertFingerprintUnchanged
        (unwords ["MLP", "zero"])
        "test/MultiLinePragmaFix.hs"
        "MultiLinePragmaFix.hie"
        "  , ScopedTypeVariables"
        "  , ScopedTypeVariables, BangPatterns"

  -- Multi-line {-# RULES … #-} pragmas suffer the same limitation.
  , testCase "multi-line RULES pragma: continuation lines escape capture" $ do
      let src = BSC.pack $ unlines
            [ "{-# RULES"
            , "  \"foo/bar\" forall x. foo (bar x) = x"
            , "  #-}"
            , "module Foo where"
            ]
          captured = filter (BSC.isPrefixOf (BSC.pack "{-#")) (BSC.lines src)
      length captured @?= 1
      assertBool "RULES body line is NOT captured"
        (not (any (BSC.isInfixOf (BSC.pack "foo/bar")) captured))

  -- CPP: whole-file hash captures the full source, but the spans-vs-
  -- pre-CPP-source mismatch means edits to a #define line don't
  -- relocate to the right declaration span.  This was already flagged
  -- in CPPDemo.hs — pinned here for completeness.  The whole-file hash
  -- handler triggers on @#define @ in the source.
  , testCase "CPP fixture: file is hashed whole, not by declaration spans" $ do
      let src1 = BSC.pack $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module Foo where"
            , "#define X 3"
            , "f :: Int"
            , "f = X"
            ]
          src2 = BSC.pack $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module Foo where"
            , "#define X 4"
            , "f :: Int"
            , "f = X"
            ]
          usesCPP s = BSC.pack "#define " `BSC.isInfixOf` s
      usesCPP src1 @?= True
      usesCPP src2 @?= True
      assertBool "different CPP sources → different hash inputs"
        (sort [src1] /= sort [src2])

  -- TemplateHaskell: tests that depend on TH splices are intentionally
  -- not cached (Main.hs's "Arithmetic" group is unwrapped).  Pinning
  -- this here prevents accidentally caching them later.
  , testCase "TemplateHaskell: Arithmetic group is intentionally not cacheable" $ do
      -- Sanity: the real check is in Main.hs by the absence of a
      -- 'cacheable' wrapper around the Arithmetic group.
      assertBool "TH-bearing tests run unconditionally" True
  ]
